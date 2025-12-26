import UIKit
import MetalKit

public final class LiquidToggleGlassView: UIView {
    private var metalView: MTKView?
    private var renderer: ToggleGlassRenderer?
    private var displayLink: CADisplayLink?

    public var cornerRadius: CGFloat = 16
    public var refractionStrength: Float = 0.4
    public var minificationStrength: Float = 0.6
    public var chromeStrength: Float = 1
    public var glassTintStrength: Float = 0.1
    public var edgeBandMultiplier: Float = 0.25

    private var morphProgress: Float = 0.0
    private var morphVelocity: Float = 0.0
    private var targetMorph: Float = 0.0

    private let springStiffness: Float = 80.0
    private let springDamping: Float = 12.0

    public var activeExpansionH: CGFloat = 12.0
    public var activeExpansionV: CGFloat = 6.0

    public var instantPositionTracking: Bool = false
    public var enableLiquidStretch: Bool = false

    private var baseFrame: CGRect = .zero
    private var currentBaseFrame: CGRect = .zero
    private var frameVelocity: CGPoint = .zero

    private var lastBaseX: CGFloat = 0
    private var stretchAmount: CGFloat = 0
    private var stretchVelocity: CGFloat = 0
    private let stretchStiffness: CGFloat = 200.0
    private let stretchDamping: CGFloat = 15.0
    private let maxStretch: CGFloat = 0.15

    private var overdragAmount: CGFloat = 0
    private var verticalDragAmount: CGFloat = 0

    private weak var sourceView: UIView?

    private var isActive: Bool = false

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

        renderer = ToggleGlassRenderer(device: device, metalView: mtkView)
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

    public func setSourceView(_ view: UIView) {
        self.sourceView = view
    }

    public func setBaseFrame(_ frame: CGRect) {
        if currentBaseFrame == .zero {
            currentBaseFrame = frame
        }
        self.baseFrame = frame
        if !isActive && morphProgress < 0.01 {
            self.frame = frame
            self.currentBaseFrame = frame
        }
    }

    public func setOverdrag(_ amount: CGFloat) {
        self.overdragAmount = max(-1, min(1, amount))
    }

    public func setVerticalDrag(_ amount: CGFloat) {
        self.verticalDragAmount = max(-1, min(1, amount))
    }

    public func setActive(_ active: Bool) {
        self.isActive = active
        self.targetMorph = active ? 1.0 : 0.0
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

        if instantPositionTracking {
            currentBaseFrame = baseFrame
            frameVelocity = .zero
        } else {
            let posStiffness: CGFloat = 300.0
            let posDamping: CGFloat = 22.0

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
        }

        var currentStretch: CGFloat = 0
        if enableLiquidStretch {
            let velocityX = (baseFrame.origin.x - lastBaseX) / dt
            lastBaseX = baseFrame.origin.x

            let targetStretch = max(-maxStretch, min(maxStretch, velocityX * 0.0008))

            let stretchDisplacement = stretchAmount - targetStretch
            let stretchForce = -stretchStiffness * stretchDisplacement
            let stretchDamp = -stretchDamping * stretchVelocity
            let stretchAccel = stretchForce + stretchDamp

            stretchVelocity += stretchAccel * dt
            stretchAmount += stretchVelocity * dt
            stretchAmount = max(-maxStretch, min(maxStretch, stretchAmount))

            if abs(stretchDisplacement) < 0.001 && abs(stretchVelocity) < 0.01 {
                stretchAmount = targetStretch
                stretchVelocity = 0
            }

            currentStretch = stretchAmount * CGFloat(morphProgress)
        }

        let expansionH = activeExpansionH * CGFloat(morphProgress)
        let expansionV = activeExpansionV * CGFloat(morphProgress)

        let baseWidth = currentBaseFrame.width + expansionH * 2
        let baseHeight = currentBaseFrame.height + expansionV * 2
        let stretchPixels = baseWidth * currentStretch
        var leftExpansion = expansionH - (currentStretch > 0 ? stretchPixels * 0.3 : -stretchPixels * 0.7)
        var rightExpansion = expansionH + (currentStretch > 0 ? stretchPixels * 0.7 : -stretchPixels * 0.3)

        let overdragStretch = abs(overdragAmount) * baseWidth * 0.15 * CGFloat(morphProgress)
        let overdragOffset = overdragAmount * 6.0 * CGFloat(morphProgress)
        if overdragAmount < 0 {
            leftExpansion += overdragStretch
        } else if overdragAmount > 0 {
            rightExpansion += overdragStretch
        }

        let verticalStretch = abs(verticalDragAmount) * baseHeight * 0.2 * CGFloat(morphProgress)
        let verticalOffset = verticalDragAmount * 4.0 * CGFloat(morphProgress)
        var topExpansion = expansionV
        var bottomExpansion = expansionV
        if verticalDragAmount < 0 {
            topExpansion += verticalStretch
        } else if verticalDragAmount > 0 {
            bottomExpansion += verticalStretch
        }

        let expandedFrame = CGRect(
            x: currentBaseFrame.origin.x - leftExpansion + overdragOffset,
            y: currentBaseFrame.origin.y - topExpansion + verticalOffset,
            width: currentBaseFrame.width + leftExpansion + rightExpansion,
            height: currentBaseFrame.height + topExpansion + bottomExpansion
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
        let source = sourceView ?? windowView

        let frameInSource = convert(bounds, to: source)
        let scale: CGFloat = 2.0

        let padding: CGFloat = 30.0
        var captureRect = frameInSource.insetBy(dx: -padding, dy: -padding)
        captureRect = captureRect.intersection(source.bounds)

        guard captureRect.width > 0, captureRect.height > 0 else { return }

        let wasHidden = isHidden
        isHidden = true

        UIGraphicsBeginImageContextWithOptions(captureRect.size, false, scale)
        defer {
            UIGraphicsEndImageContext()
            isHidden = wasHidden
        }

        guard let ctx = UIGraphicsGetCurrentContext() else { return }

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

private final class ToggleGlassRenderer: NSObject, MTKViewDelegate {
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
            edgeFactor = edgeFactor * edgeFactor * 2;
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
            float minify = max(0.0, 1.0 - distNorm * distNorm) * morph;
            float2 minifyOffset = normFromCenter * minify * uniforms.minificationStrength * min(halfWidth, halfHeight);

            float disp = effectEdgeFactor * uniforms.refractionStrength * edgeBand * 0.5;
            float chrome = effectEdgeFactor * uniforms.chromeStrength;

            float2 redOff = toCenter * (disp + chrome) + minifyOffset;
            float2 greenOff = toCenter * disp + minifyOffset;
            float2 blueOff = toCenter * (disp - chrome) + minifyOffset;

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
