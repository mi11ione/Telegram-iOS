import UIKit
import MetalKit

public final class LiquidGlassView: UIView {
    private weak var sourceView: UIView?
    private var metalView: MTKView?
    private var renderer: LiquidGlassRenderer?
    private var physicsDisplayLink: CADisplayLink?

    public var cornerRadius: CGFloat = 32
    public var refractionStrength: Float = 0.4
    public var isDarkMode: Bool = false

    public var metalDevice: MTLDevice? {
        return metalView?.device
    }

    private var stretchOffset: CGPoint = .zero
    private var stretchVelocity: CGPoint = .zero
    private var targetStretch: CGPoint = .zero

    private let stretchStiffness: CGFloat = 350.0
    private let stretchDamping: CGFloat = 20.0
    private let maxStretch: CGFloat = 0.35

    public var stretchEnabled: Bool = true

    private let stretchSensitivity: CGFloat = 0.008
    private var touchStartPoint: CGPoint?
    private var isTrackingTouch: Bool = false

    private var lastBlurredImage: UIImage?
    private var lastCaptureRect: CGSize = .zero
    private var lastOffsetInCapture: CGPoint = .zero

    public init(sourceView: UIView) {
        self.sourceView = sourceView
        super.init(frame: .zero)
        setup()
    }

    public override init(frame: CGRect) {
        super.init(frame: frame)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    deinit {
        SharedGlassCapture.shared.unregister(self)
        physicsDisplayLink?.invalidate()
    }

    public func setSourceView(_ view: UIView) {
        self.sourceView = view
    }

    private let stretchPadding: CGFloat = 20.0

    private func setup() {
        guard let device = MTLCreateSystemDefaultDevice() else { return }

        self.clipsToBounds = false

        let mtkView = MTKView()
        mtkView.device = device
        mtkView.backgroundColor = .clear
        mtkView.isOpaque = false
        mtkView.framebufferOnly = false
        mtkView.isPaused = true
        mtkView.enableSetNeedsDisplay = true
        addSubview(mtkView)
        self.metalView = mtkView

        renderer = LiquidGlassRenderer(device: device, metalView: mtkView)
        mtkView.delegate = renderer
    }

    public override func didMoveToWindow() {
        super.didMoveToWindow()
        if let window = window {
            var effectiveSource: UIView = window
            if let existingSource = sourceView {
                effectiveSource = existingSource
            } else if let rootVC = window.rootViewController {
                effectiveSource = rootVC.view
                self.sourceView = effectiveSource
            }

            SharedGlassCapture.shared.register(self, sourceView: effectiveSource)
            startPhysicsDisplayLink()
        } else {
            SharedGlassCapture.shared.unregister(self)
            stopPhysicsDisplayLink()
        }
    }

    public override func layoutSubviews() {
        super.layoutSubviews()
        metalView?.frame = bounds.insetBy(dx: -stretchPadding, dy: -stretchPadding)
    }

    public override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        super.touchesBegan(touches, with: event)
        guard stretchEnabled, let touch = touches.first else { return }

        let location = touch.location(in: self)
        touchStartPoint = location
        isTrackingTouch = true
    }

    public override func touchesMoved(_ touches: Set<UITouch>, with event: UIEvent?) {
        super.touchesMoved(touches, with: event)
        guard stretchEnabled, isTrackingTouch, let startPoint = touchStartPoint, let touch = touches.first else { return }

        let location = touch.location(in: self)
        let dx = location.x - startPoint.x
        let dy = location.y - startPoint.y

        let stretchX = dx * stretchSensitivity
        let stretchY = dy * stretchSensitivity

        setStretchImmediate(CGPoint(x: stretchX, y: stretchY))
    }

    public override func touchesEnded(_ touches: Set<UITouch>, with event: UIEvent?) {
        super.touchesEnded(touches, with: event)
        endTouchTracking()
    }

    public override func touchesCancelled(_ touches: Set<UITouch>, with event: UIEvent?) {
        super.touchesCancelled(touches, with: event)
        endTouchTracking()
    }

    private func endTouchTracking() {
        touchStartPoint = nil
        isTrackingTouch = false
        releaseStretch()
    }

    private func startPhysicsDisplayLink() {
        guard physicsDisplayLink == nil else { return }
        let link = CADisplayLink(target: self, selector: #selector(physicsTick))
        link.preferredFramesPerSecond = 60
        link.add(to: .main, forMode: .common)
        physicsDisplayLink = link
    }

    private func stopPhysicsDisplayLink() {
        physicsDisplayLink?.invalidate()
        physicsDisplayLink = nil
    }

    @objc private func physicsTick(_ link: CADisplayLink) {
        guard stretchEnabled else { return }

        let dt = link.targetTimestamp - link.timestamp
        guard dt > 0.001 && dt < 0.1 else { return }

        let dxTarget = targetStretch.x - stretchOffset.x
        let forceX = stretchStiffness * dxTarget - stretchDamping * stretchVelocity.x
        stretchVelocity.x += forceX * dt
        stretchOffset.x += stretchVelocity.x * dt
        stretchOffset.x = max(-maxStretch, min(maxStretch, stretchOffset.x))

        let dyTarget = targetStretch.y - stretchOffset.y
        let forceY = stretchStiffness * dyTarget - stretchDamping * stretchVelocity.y
        stretchVelocity.y += forceY * dt
        stretchOffset.y += stretchVelocity.y * dt
        stretchOffset.y = max(-maxStretch, min(maxStretch, stretchOffset.y))

        if abs(dxTarget) < 0.001 && abs(dyTarget) < 0.001 &&
           abs(stretchVelocity.x) < 0.01 && abs(stretchVelocity.y) < 0.01 {
            stretchOffset = targetStretch
            stretchVelocity = .zero
        }

        if (stretchOffset.x != 0 || stretchOffset.y != 0 || stretchVelocity.x != 0 || stretchVelocity.y != 0),
           let image = lastBlurredImage {
            renderWithStretch(blurredImage: image, captureRect: lastCaptureRect, offsetInCapture: lastOffsetInCapture)
        }
    }

    public func setStretchTarget(_ offset: CGPoint) {
        targetStretch = CGPoint(
            x: max(-maxStretch, min(maxStretch, offset.x)),
            y: max(-maxStretch, min(maxStretch, offset.y))
        )
    }

    public func setStretchImmediate(_ offset: CGPoint) {
        let clamped = CGPoint(
            x: max(-maxStretch, min(maxStretch, offset.x)),
            y: max(-maxStretch, min(maxStretch, offset.y))
        )
        stretchOffset = clamped
        targetStretch = clamped

        if let image = lastBlurredImage {
            renderWithStretch(blurredImage: image, captureRect: lastCaptureRect, offsetInCapture: lastOffsetInCapture)
        }
    }

    public func releaseStretch() {
        targetStretch = .zero
    }

    func updateWithSharedCapture(
        blurredImage: UIImage,
        captureRect: CGSize,
        offsetInCapture: CGPoint
    ) {
        lastBlurredImage = blurredImage
        lastCaptureRect = captureRect
        lastOffsetInCapture = offsetInCapture

        renderWithStretch(blurredImage: blurredImage, captureRect: captureRect, offsetInCapture: offsetInCapture)
    }

    private func renderWithStretch(blurredImage: UIImage, captureRect: CGSize, offsetInCapture: CGPoint) {
        guard let renderer = renderer else { return }
        guard bounds.width > 0, bounds.height > 0 else { return }

        let renderSize = CGSize(
            width: bounds.width + stretchPadding * 2,
            height: bounds.height + stretchPadding * 2
        )

        renderer.updateUniforms(
            size: bounds.size,
            renderSize: renderSize,
            padding: stretchPadding,
            offset: offsetInCapture,
            backgroundSize: captureRect,
            cornerRadius: cornerRadius,
            refractionStrength: refractionStrength,
            isDarkMode: traitCollection.userInterfaceStyle == .dark,
            stretchOffset: stretchOffset,
            backgroundImage: blurredImage
        )

        metalView?.setNeedsDisplay()
    }
}

private final class LiquidGlassRenderer: NSObject, MTKViewDelegate {
    private let device: MTLDevice
    private let commandQueue: MTLCommandQueue
    private var pipelineState: MTLRenderPipelineState?
    private var backgroundTexture: MTLTexture?

    private let textureLoader: MTKTextureLoader

    private var uniforms = Uniforms()

    struct Uniforms {
        var size: SIMD2<Float> = .zero
        var renderSize: SIMD2<Float> = .zero
        var padding: Float = 0
        var offset: SIMD2<Float> = .zero
        var backgroundSize: SIMD2<Float> = .zero
        var cornerRadius: Float = 0
        var refractionStrength: Float = 0
        var isDarkMode: Float = 0
        var stretchOffset: SIMD2<Float> = .zero
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
            float2 renderSize;
            float padding;
            float2 offset;
            float2 backgroundSize;
            float cornerRadius;
            float refractionStrength;
            float isDarkMode;
            float2 stretchOffset;
        };

        float sdRoundedBox(float2 p, float2 b, float r) {
            float2 q = abs(p) - b + r;
            return min(max(q.x, q.y), 0.0) + length(max(q, 0.0)) - r;
        }

        float2 applyStretchDeformation(float2 p, float2 center, float2 halfSize, float2 stretch) {
            float2 fromCenter = p - center;
            float2 normalized = fromCenter / halfSize;

            float dist = length(normalized);

            float2 stretchDir = normalize(stretch + 0.0001);
            float alignment = dot(normalize(fromCenter + 0.0001), stretchDir);

            float stretchMag = length(stretch);
            float influence = (1.0 + alignment) * 0.5;

            float edgeFactor = smoothstep(0.0, 1.0, dist);
            float2 deformation = stretch * halfSize * edgeFactor * influence * 1.5;

            return p + deformation;
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
            float2 renderPixelPos = uv * uniforms.renderSize;
            float2 pixelPos = renderPixelPos - float2(uniforms.padding);

            float2 center = uniforms.size * 0.5;
            float halfWidth = uniforms.size.x * 0.5;
            float halfHeight = uniforms.size.y * 0.5;
            float2 halfSize = float2(halfWidth, halfHeight);

            float2 deformedPos = pixelPos;
            float stretchMag = length(uniforms.stretchOffset);
            if (stretchMag > 0.001) {
                float2 fromCenter = pixelPos - center;
                float2 normalized = fromCenter / halfSize;
                float dist = length(normalized);

                float2 stretchDir = normalize(uniforms.stretchOffset);
                float alignment = dot(normalize(fromCenter + 0.0001), stretchDir);

                float influence = (1.0 + alignment) * 0.5;
                float edgeFactor = smoothstep(0.0, 1.0, dist);

                float2 invDeformation = uniforms.stretchOffset * halfSize * edgeFactor * influence * 2.5;
                deformedPos = pixelPos - invDeformation;
            }

            float2 fromCenter = deformedPos - center;
            float sdf = sdRoundedBox(fromCenter, halfSize, uniforms.cornerRadius);

            float distFromEdge = -sdf;
            float edgeBand = min(uniforms.size.x, uniforms.size.y) * 0.3;

            float edgeFactor = 1.0 - smoothstep(0.0, edgeBand, distFromEdge);
            edgeFactor = edgeFactor * edgeFactor * edgeFactor * 2;

            float2 nearestCenterPoint;
            float insetX = min(uniforms.cornerRadius, halfWidth);
            float insetY = min(uniforms.cornerRadius, halfHeight);

            if (uniforms.size.x >= uniforms.size.y) {
                float clampedX = clamp(deformedPos.x, insetX, uniforms.size.x - insetX);
                nearestCenterPoint = float2(clampedX, center.y);
            } else {
                float clampedY = clamp(deformedPos.y, insetY, uniforms.size.y - insetY);
                nearestCenterPoint = float2(center.x, clampedY);
            }

            float2 toCenter = nearestCenterPoint - deformedPos;
            float distToCenter = length(toCenter);
            if (distToCenter > 0.001) {
                toCenter = toCenter / distToCenter;
            } else {
                toCenter = float2(0.0);
            }

            float displacement = edgeFactor * uniforms.refractionStrength * edgeBand;

            float chromeStrength = edgeFactor * 4.0;
            float2 redOffset = toCenter * (displacement + chromeStrength);
            float2 greenOffset = toCenter * displacement;
            float2 blueOffset = toCenter * (displacement - chromeStrength);

            float2 redUV = (uniforms.offset + pixelPos + redOffset) / uniforms.backgroundSize;
            float2 greenUV = (uniforms.offset + pixelPos + greenOffset) / uniforms.backgroundSize;
            float2 blueUV = (uniforms.offset + pixelPos + blueOffset) / uniforms.backgroundSize;

            redUV = clamp(redUV, float2(0.001), float2(0.999));
            greenUV = clamp(greenUV, float2(0.001), float2(0.999));
            blueUV = clamp(blueUV, float2(0.001), float2(0.999));

            if (sdf > 0.0) {
                discard_fragment();
            }

            float r = backgroundTexture.sample(texSampler, redUV).r;
            float g = backgroundTexture.sample(texSampler, greenUV).g;
            float b = backgroundTexture.sample(texSampler, blueUV).b;
            float3 color = float3(r, g, b);

            float3 tint = uniforms.isDarkMode > 0.5
                ? float3(0.15, 0.15, 0.15)
                : float3(1.0, 1.0, 1.0);
            color = mix(color, tint, 0.8);

            float aa = 1.0 - smoothstep(-1.0, 0.5, sdf);

            return float4(color.rgb, aa);
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
        renderSize: CGSize,
        padding: CGFloat,
        offset: CGPoint,
        backgroundSize: CGSize,
        cornerRadius: CGFloat,
        refractionStrength: Float,
        isDarkMode: Bool,
        stretchOffset: CGPoint,
        backgroundImage: UIImage
    ) {
        let scale = UIScreen.main.scale

        uniforms.size = SIMD2<Float>(Float(size.width * scale), Float(size.height * scale))
        uniforms.renderSize = SIMD2<Float>(Float(renderSize.width * scale), Float(renderSize.height * scale))
        uniforms.padding = Float(padding * scale)
        uniforms.offset = SIMD2<Float>(Float(offset.x * scale), Float(offset.y * scale))
        uniforms.backgroundSize = SIMD2<Float>(Float(backgroundSize.width * scale), Float(backgroundSize.height * scale))
        uniforms.cornerRadius = Float(cornerRadius * scale)
        uniforms.refractionStrength = refractionStrength
        uniforms.isDarkMode = isDarkMode ? 1.0 : 0.0
        uniforms.stretchOffset = SIMD2<Float>(Float(stretchOffset.x), Float(stretchOffset.y))

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
