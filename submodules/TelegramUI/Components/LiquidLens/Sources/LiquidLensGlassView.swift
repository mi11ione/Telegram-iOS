import UIKit
import MetalKit

public final class LiquidLensGlassView: UIView {
    private var metalView: MTKView?
    private var renderer: LensGlassRenderer?
    private var displayLink: CADisplayLink?

    public var cornerRadius: CGFloat = 24
    public var refractionStrength: Float = 0.22
    public var minificationStrength: Float = 0.08
    public var chromeStrength: Float = 3.0
    public var glassTintStrength: Float = 0.12
    public var edgeBandMultiplier: Float = 0.3

    private var morphProgress: Float = 0.0
    private var morphVelocity: Float = 0.0
    private var targetMorph: Float = 0.0

    private let springStiffness: Float = 60.0
    private let springDamping: Float = 10.0

    private let activeExpansionPixels: CGFloat = 8.0

    private var stretchAmount: CGFloat = 0
    private var stretchVelocity: CGFloat = 0
    private let stretchStiffness: CGFloat = 200.0
    private let stretchDamping: CGFloat = 14.0
    private let maxStretch: CGFloat = 0.2

    private weak var tabBarContentView: UIView?

    private var isLifted: Bool = false

    private var baseFrame: CGRect = .zero
    private var currentBaseFrame: CGRect = .zero
    private var frameVelocity: CGPoint = .zero

    public override init(frame: CGRect) {
        super.init(frame: frame)
        setup()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    deinit {
        displayLink?.invalidate()
    }

    private func setup() {
        guard let device = MTLCreateSystemDefaultDevice() else { return }

        let mtkView = MTKView()
        mtkView.device = device
        mtkView.backgroundColor = .clear
        mtkView.isOpaque = false
        mtkView.framebufferOnly = false
        mtkView.isPaused = true
        mtkView.enableSetNeedsDisplay = true
        addSubview(mtkView)
        self.metalView = mtkView

        renderer = LensGlassRenderer(device: device, metalView: mtkView)
        mtkView.delegate = renderer
    }

    public override func didMoveToWindow() {
        super.didMoveToWindow()
        if window != nil {
            startDisplayLink()
        } else {
            stopDisplayLink()
        }
    }

    public override func layoutSubviews() {
        super.layoutSubviews()
        metalView?.frame = bounds
    }

    public func setTabBarContentView(_ view: UIView) {
        self.tabBarContentView = view
    }

    public func setLifted(_ lifted: Bool) {
        self.isLifted = lifted
        self.targetMorph = lifted ? 1.0 : 0.0
    }

    public func setBaseFrame(_ frame: CGRect) {
        if currentBaseFrame == .zero {
            currentBaseFrame = frame
        }

        let oldCenter = CGPoint(x: baseFrame.midX, y: baseFrame.midY)
        let newCenter = CGPoint(x: frame.midX, y: frame.midY)
        let deltaX = newCenter.x - oldCenter.x
        let distance = hypot(deltaX, newCenter.y - oldCenter.y)

        if distance > 5.0 && baseFrame.width > 0 {
            let impulse = max(-maxStretch, min(maxStretch, deltaX * 0.0012))
            stretchVelocity += impulse * 12.0
        }

        self.baseFrame = frame

        if morphProgress < 0.01 {
            self.frame = frame
            self.currentBaseFrame = frame
        }
    }

    private func startDisplayLink() {
        guard displayLink == nil else { return }
        let link = CADisplayLink(target: self, selector: #selector(tick))
        link.preferredFramesPerSecond = 60
        link.add(to: .main, forMode: .common)
        displayLink = link
    }

    private func stopDisplayLink() {
        displayLink?.invalidate()
        displayLink = nil
    }

    @objc private func tick(_ link: CADisplayLink) {
        let dt = link.targetTimestamp - link.timestamp
        guard dt > 0.001 && dt < 0.1 else { return }

        targetMorph = isLifted ? 1.0 : 0.0

        let displacement = morphProgress - targetMorph
        let springForce = -springStiffness * displacement
        let dampingForce = -springDamping * morphVelocity
        let acceleration = springForce + dampingForce

        morphVelocity += acceleration * Float(dt)
        morphProgress += morphVelocity * Float(dt)
        morphProgress = max(0.0, min(1.0, morphProgress))

        if abs(displacement) < 0.002 && abs(morphVelocity) < 0.01 {
            morphProgress = targetMorph
            morphVelocity = 0
        }

        let posStiffness: CGFloat = 450.0
        let posDamping: CGFloat = 28.0

        let dx = baseFrame.origin.x - currentBaseFrame.origin.x
        let dy = baseFrame.origin.y - currentBaseFrame.origin.y

        let forceX = posStiffness * dx - posDamping * frameVelocity.x
        let forceY = posStiffness * dy - posDamping * frameVelocity.y

        frameVelocity.x += forceX * dt
        frameVelocity.y += forceY * dt

        currentBaseFrame.origin.x += frameVelocity.x * dt
        currentBaseFrame.origin.y += frameVelocity.y * dt
        currentBaseFrame.size = baseFrame.size

        if abs(dx) < 0.5 && abs(dy) < 0.5 && abs(frameVelocity.x) < 1 && abs(frameVelocity.y) < 1 {
            currentBaseFrame = baseFrame
            frameVelocity = .zero
        }

        let stretchForce = -stretchStiffness * stretchAmount
        let stretchDamp = -stretchDamping * stretchVelocity
        let stretchAccel = stretchForce + stretchDamp

        stretchVelocity += stretchAccel * dt
        stretchAmount += stretchVelocity * dt
        stretchAmount = max(-maxStretch, min(maxStretch, stretchAmount))

        if abs(stretchAmount) < 0.001 && abs(stretchVelocity) < 0.01 {
            stretchAmount = 0
            stretchVelocity = 0
        }

        let currentStretch = stretchAmount * CGFloat(morphProgress)

        let expansion = activeExpansionPixels * CGFloat(morphProgress)
        let baseWidth = currentBaseFrame.width + expansion * 2
        let stretchPixels = baseWidth * currentStretch

        var leftExpansion = expansion - (currentStretch > 0 ? stretchPixels * 0.3 : -stretchPixels * 0.7)
        var rightExpansion = expansion + (currentStretch > 0 ? stretchPixels * 0.7 : -stretchPixels * 0.3)

        leftExpansion = max(0, leftExpansion)
        rightExpansion = max(0, rightExpansion)

        let expandedFrame = CGRect(
            x: currentBaseFrame.origin.x - leftExpansion,
            y: currentBaseFrame.origin.y - expansion,
            width: currentBaseFrame.width + leftExpansion + rightExpansion,
            height: currentBaseFrame.height + expansion * 2
        )

        if frame != expandedFrame {
            frame = expandedFrame
        }

        cornerRadius = expandedFrame.height / 2.0

        alpha = CGFloat(morphProgress)

        if morphProgress > 0.01 {
            captureAndRender()
        }
    }

    private func captureAndRender() {
        guard let renderer = renderer else { return }
        guard bounds.width > 0, bounds.height > 0 else { return }

        guard let windowView = window else { return }
        let sourceView = tabBarContentView ?? superview
        guard let source = sourceView else { return }

        let frameInSource = convert(bounds, to: source)
        let scale: CGFloat = 2.0

        let hPadding: CGFloat = 30.0
        var captureRect = CGRect(
            x: frameInSource.origin.x - hPadding,
            y: frameInSource.origin.y,
            width: frameInSource.width + hPadding * 2,
            height: frameInSource.height
        )
        captureRect = captureRect.intersection(source.bounds)

        guard captureRect.width > 0, captureRect.height > 0 else { return }

        let wasHidden = isHidden
        isHidden = true

        UIGraphicsBeginImageContextWithOptions(captureRect.size, true, scale)
        defer {
            UIGraphicsEndImageContext()
            isHidden = wasHidden
        }

        guard let ctx = UIGraphicsGetCurrentContext() else { return }

        let sourceOriginInWindow = source.convert(CGPoint.zero, to: windowView)

        ctx.saveGState()
        ctx.translateBy(x: -(captureRect.origin.x + sourceOriginInWindow.x),
                        y: -(captureRect.origin.y + sourceOriginInWindow.y))
        windowView.layer.render(in: ctx)
        ctx.restoreGState()

        let isDarkMode = traitCollection.userInterfaceStyle == .dark
        let overlayColor = isDarkMode
            ? UIColor(white: 0.1, alpha: 0.55)
            : UIColor(white: 1.0, alpha: 0.85)
        ctx.setFillColor(overlayColor.cgColor)
        ctx.fill(CGRect(origin: .zero, size: captureRect.size))

        ctx.translateBy(x: -captureRect.origin.x, y: -captureRect.origin.y)
        source.layer.render(in: ctx)

        guard let snapshot = UIGraphicsGetImageFromCurrentImageContext() else { return }

        let offsetInCapture = CGPoint(
            x: frameInSource.origin.x - captureRect.origin.x,
            y: frameInSource.origin.y - captureRect.origin.y
        )

        renderer.updateUniforms(
            size: bounds.size,
            offset: offsetInCapture,
            backgroundSize: captureRect.size,
            cornerRadius: cornerRadius,
            refractionStrength: refractionStrength,
            minificationStrength: minificationStrength,
            chromeStrength: chromeStrength,
            glassTintStrength: glassTintStrength,
            edgeBandMultiplier: edgeBandMultiplier,
            morphProgress: morphProgress,
            backgroundImage: snapshot
        )

        metalView?.setNeedsDisplay()
    }
}

private final class LensGlassRenderer: NSObject, MTKViewDelegate {
    private let device: MTLDevice
    private let commandQueue: MTLCommandQueue
    private let textureLoader: MTKTextureLoader
    private var pipelineState: MTLRenderPipelineState?
    private var backgroundTexture: MTLTexture?

    private static let screenScale = Float(UIScreen.main.scale)

    private var uniforms = Uniforms()

    struct Uniforms {
        var size: SIMD2<Float> = .zero
        var offset: SIMD2<Float> = .zero
        var backgroundSize: SIMD2<Float> = .zero
        var cornerRadius: Float = 0
        var refractionStrength: Float = 0
        var minificationStrength: Float = 0
        var chromeStrength: Float = 0
        var glassTintStrength: Float = 0
        var edgeBandMultiplier: Float = 0
        var morphProgress: Float = 0
    }

    init(device: MTLDevice, metalView: MTKView) {
        self.device = device
        self.commandQueue = device.makeCommandQueue()!
        self.textureLoader = MTKTextureLoader(device: device)
        super.init()
        setupPipeline(metalView: metalView)
    }

    private func setupPipeline(metalView: MTKView) {
        let shaderSource = """
        #include <metal_stdlib>
        using namespace metal;

        struct VertexOut {
            float4 position [[position]];
            float2 texCoord;
        };

        struct Uniforms {
            float2 size;
            float2 offset;
            float2 backgroundSize;
            float cornerRadius;
            float refractionStrength;
            float minificationStrength;
            float chromeStrength;
            float glassTintStrength;
            float edgeBandMultiplier;
            float morphProgress;
        };

        float sdRoundedBox(float2 p, float2 b, float r) {
            float2 q = abs(p) - b + r;
            return min(max(q.x, q.y), 0.0) + length(max(q, 0.0)) - r;
        }

        vertex VertexOut vertexShader(uint vertexID [[vertex_id]]) {
            float2 positions[4] = {
                float2(-1.0, -1.0),
                float2( 1.0, -1.0),
                float2(-1.0,  1.0),
                float2( 1.0,  1.0)
            };
            float2 texCoords[4] = {
                float2(0.0, 1.0),
                float2(1.0, 1.0),
                float2(0.0, 0.0),
                float2(1.0, 0.0)
            };
            VertexOut out;
            out.position = float4(positions[vertexID], 0.0, 1.0);
            out.texCoord = texCoords[vertexID];
            return out;
        }

        fragment float4 fragmentShader(
            VertexOut in [[stage_in]],
            texture2d<float, access::sample> backgroundTexture [[texture(0)]],
            constant Uniforms &uniforms [[buffer(0)]]
        ) {
            constexpr sampler texSampler(coord::normalized, address::clamp_to_edge, filter::linear);

            float2 uv = in.texCoord;
            float2 pixelPos = uv * uniforms.size;

            float2 center = uniforms.size * 0.5;
            float2 fromCenter = pixelPos - center;

            float halfWidth = uniforms.size.x * 0.5;
            float halfHeight = uniforms.size.y * 0.5;
            float sdf = sdRoundedBox(fromCenter, float2(halfWidth, halfHeight), uniforms.cornerRadius);

            if (sdf > 0.0) {
                discard_fragment();
            }

            float morph = uniforms.morphProgress;
            morph = morph * morph * (3.0 - 2.0 * morph);

            float distFromEdge = -sdf;
            float edgeBand = min(uniforms.size.x, uniforms.size.y) * uniforms.edgeBandMultiplier;

            float edgeFactor = 1.0 - smoothstep(0.0, edgeBand, distFromEdge);
            edgeFactor = edgeFactor * edgeFactor * edgeFactor * 2.0;
            float effectEdgeFactor = edgeFactor * morph;

            float2 nearestCenterPoint;
            float insetX = min(uniforms.cornerRadius, halfWidth);
            float insetY = min(uniforms.cornerRadius, halfHeight);

            if (uniforms.size.x >= uniforms.size.y) {
                float clampedX = clamp(pixelPos.x, insetX, uniforms.size.x - insetX);
                nearestCenterPoint = float2(clampedX, center.y);
            } else {
                float clampedY = clamp(pixelPos.y, insetY, uniforms.size.y - insetY);
                nearestCenterPoint = float2(center.x, clampedY);
            }

            float2 toCenter = nearestCenterPoint - pixelPos;
            float distToCenter = length(toCenter);
            toCenter = distToCenter > 0.001 ? toCenter / distToCenter : float2(0.0);

            float2 normFromCenter = fromCenter / float2(halfWidth, halfHeight);
            float distNorm = length(normFromCenter);
            float magnify = max(0.0, 1.0 - distNorm * distNorm) * morph;
            float2 magOffset = -normFromCenter * magnify * uniforms.minificationStrength * min(halfWidth, halfHeight);

            float disp = effectEdgeFactor * uniforms.refractionStrength * edgeBand;
            float chrome = effectEdgeFactor * uniforms.chromeStrength;

            float2 redOff = toCenter * (disp + chrome) + magOffset;
            float2 greenOff = toCenter * disp + magOffset;
            float2 blueOff = toCenter * (disp - chrome) + magOffset;

            float2 redUV = (uniforms.offset + pixelPos + redOff) / uniforms.backgroundSize;
            float2 greenUV = (uniforms.offset + pixelPos + greenOff) / uniforms.backgroundSize;
            float2 blueUV = (uniforms.offset + pixelPos + blueOff) / uniforms.backgroundSize;

            redUV = clamp(redUV, float2(0.001), float2(0.999));
            greenUV = clamp(greenUV, float2(0.001), float2(0.999));
            blueUV = clamp(blueUV, float2(0.001), float2(0.999));

            float r = backgroundTexture.sample(texSampler, redUV).r;
            float g = backgroundTexture.sample(texSampler, greenUV).g;
            float b = backgroundTexture.sample(texSampler, blueUV).b;
            float3 glassColor = float3(r, g, b);

            float3 color = mix(glassColor, float3(1.0), uniforms.glassTintStrength);

            float overallShadow = 0.02 * morph;
            float edgeShadow = smoothstep(20.0, 0.0, distFromEdge) * 0.05 * morph;
            color = color * (1.0 - overallShadow - edgeShadow);

            float normalizedY = -fromCenter.y / halfHeight;
            float topHighlight = max(0.0, normalizedY) * 0.04 * morph;
            color = mix(color, float3(1.0), topHighlight);

            float aa = 1.0 - smoothstep(-1.0, 0.5, sdf);
            return float4(color, aa);
        }
        """

        guard let library = try? device.makeLibrary(source: shaderSource, options: nil),
              let vertexFunc = library.makeFunction(name: "vertexShader"),
              let fragmentFunc = library.makeFunction(name: "fragmentShader") else {
            return
        }

        let descriptor = MTLRenderPipelineDescriptor()
        descriptor.vertexFunction = vertexFunc
        descriptor.fragmentFunction = fragmentFunc
        descriptor.colorAttachments[0].pixelFormat = metalView.colorPixelFormat
        descriptor.colorAttachments[0].isBlendingEnabled = true
        descriptor.colorAttachments[0].sourceRGBBlendFactor = .sourceAlpha
        descriptor.colorAttachments[0].destinationRGBBlendFactor = .oneMinusSourceAlpha
        descriptor.colorAttachments[0].sourceAlphaBlendFactor = .one
        descriptor.colorAttachments[0].destinationAlphaBlendFactor = .oneMinusSourceAlpha

        pipelineState = try? device.makeRenderPipelineState(descriptor: descriptor)
    }

    func updateUniforms(
        size: CGSize,
        offset: CGPoint,
        backgroundSize: CGSize,
        cornerRadius: CGFloat,
        refractionStrength: Float,
        minificationStrength: Float,
        chromeStrength: Float,
        glassTintStrength: Float,
        edgeBandMultiplier: Float,
        morphProgress: Float,
        backgroundImage: UIImage
    ) {
        let scale = Self.screenScale

        uniforms.size = SIMD2<Float>(Float(size.width) * scale, Float(size.height) * scale)
        uniforms.offset = SIMD2<Float>(Float(offset.x) * scale, Float(offset.y) * scale)
        uniforms.backgroundSize = SIMD2<Float>(Float(backgroundSize.width) * scale, Float(backgroundSize.height) * scale)
        uniforms.cornerRadius = Float(cornerRadius) * scale
        uniforms.refractionStrength = refractionStrength
        uniforms.minificationStrength = minificationStrength
        uniforms.chromeStrength = chromeStrength
        uniforms.glassTintStrength = glassTintStrength
        uniforms.edgeBandMultiplier = edgeBandMultiplier
        uniforms.morphProgress = morphProgress

        if let cgImage = backgroundImage.cgImage {
            backgroundTexture = try? textureLoader.newTexture(cgImage: cgImage, options: [.SRGB: false])
        }
    }

    func mtkView(_ view: MTKView, drawableSizeWillChange size: CGSize) {}

    func draw(in view: MTKView) {
        guard let pipelineState = pipelineState,
              let drawable = view.currentDrawable,
              let descriptor = view.currentRenderPassDescriptor,
              let backgroundTexture = backgroundTexture else { return }

        descriptor.colorAttachments[0].clearColor = MTLClearColor(red: 0, green: 0, blue: 0, alpha: 0)
        descriptor.colorAttachments[0].loadAction = .clear

        guard let commandBuffer = commandQueue.makeCommandBuffer(),
              let encoder = commandBuffer.makeRenderCommandEncoder(descriptor: descriptor) else { return }

        encoder.setRenderPipelineState(pipelineState)
        encoder.setFragmentTexture(backgroundTexture, index: 0)
        encoder.setFragmentBytes(&uniforms, length: MemoryLayout<Uniforms>.stride, index: 0)
        encoder.drawPrimitives(type: .triangleStrip, vertexStart: 0, vertexCount: 4)
        encoder.endEncoding()

        commandBuffer.present(drawable)
        commandBuffer.commit()
    }
}
