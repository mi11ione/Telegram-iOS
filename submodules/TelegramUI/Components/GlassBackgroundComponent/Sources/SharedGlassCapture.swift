import UIKit
import MetalKit
import CoreImage

public final class SharedGlassCapture {
    public static let shared = SharedGlassCapture()

    private struct RegisteredView {
        weak var view: LiquidGlassView?
        weak var sourceView: UIView?
    }

    private var displayLink: CADisplayLink?
    private var registeredViews: [ObjectIdentifier: RegisteredView] = [:]
    private let lock = NSLock()
    private let ciContext: CIContext
    private let blurFilter: CIFilter
    private var textureLoaders: [ObjectIdentifier: MTKTextureLoader] = [:]
    private var lastFrameTime: CFTimeInterval = 0
    private let minimumFrameInterval: CFTimeInterval = 1.0 / 60.0

    private init() {
        self.ciContext = CIContext(options: [
            .useSoftwareRenderer: false,
            .priorityRequestLow: false,
            .cacheIntermediates: false
        ])

        self.blurFilter = CIFilter(name: "CIGaussianBlur")!
        self.blurFilter.setValue(2.0, forKey: kCIInputRadiusKey)
    }

    public func register(_ view: LiquidGlassView, sourceView: UIView) {
        lock.lock()
        defer { lock.unlock() }

        let id = ObjectIdentifier(view)
        registeredViews[id] = RegisteredView(view: view, sourceView: sourceView)

        if let device = view.metalDevice {
            let deviceId = ObjectIdentifier(device)
            if textureLoaders[deviceId] == nil {
                textureLoaders[deviceId] = MTKTextureLoader(device: device)
            }
        }

        startDisplayLinkIfNeeded()
    }

    public func unregister(_ view: LiquidGlassView) {
        lock.lock()
        defer { lock.unlock() }

        let id = ObjectIdentifier(view)
        registeredViews.removeValue(forKey: id)

        cleanupNilReferences()

        if registeredViews.isEmpty {
            stopDisplayLink()
        }
    }

    private func startDisplayLinkIfNeeded() {
        guard displayLink == nil else { return }

        let link = CADisplayLink(target: self, selector: #selector(tick))
        link.preferredFramesPerSecond = 60
        link.add(to: .main, forMode: .common)
        displayLink = link
    }

    private func stopDisplayLink() {
        displayLink?.invalidate()
        displayLink = nil
        lastFrameTime = 0
    }

    private func cleanupNilReferences() {
        registeredViews = registeredViews.filter { $0.value.view != nil }
    }

    @objc private func tick(_ link: CADisplayLink) {
        let currentTime = link.timestamp
        guard currentTime - lastFrameTime >= minimumFrameInterval else { return }
        lastFrameTime = currentTime

        lock.lock()
        cleanupNilReferences()

        guard !registeredViews.isEmpty else {
            lock.unlock()
            stopDisplayLink()
            return
        }

        let viewsCopy = registeredViews
        lock.unlock()

        var viewsBySource: [ObjectIdentifier: [(LiquidGlassView, UIView)]] = [:]

        for (_, registered) in viewsCopy {
            guard let view = registered.view,
                  let sourceView = registered.sourceView,
                  view.window != nil,
                  view.bounds.width > 0,
                  view.bounds.height > 0 else {
                continue
            }

            let sourceId = ObjectIdentifier(sourceView)
            if viewsBySource[sourceId] == nil {
                viewsBySource[sourceId] = []
            }
            viewsBySource[sourceId]?.append((view, sourceView))
        }

        for (_, viewsForSource) in viewsBySource {
            guard let firstSource = viewsForSource.first?.1 else { continue }
            captureAndDistribute(sourceView: firstSource, views: viewsForSource.map { $0.0 })
        }
    }

    private func captureAndDistribute(sourceView: UIView, views: [LiquidGlassView]) {
        guard !views.isEmpty else { return }

        let scale: CGFloat = 1.5
        let blurPadding: CGFloat = 20.0

        var unionRect: CGRect?
        var viewFrames: [(LiquidGlassView, CGRect)] = []

        for view in views {
            let frameInSource = view.convert(view.bounds, to: sourceView)
            let paddedFrame = frameInSource.insetBy(dx: -blurPadding, dy: -blurPadding)

            viewFrames.append((view, frameInSource))

            if let existing = unionRect {
                unionRect = existing.union(paddedFrame)
            } else {
                unionRect = paddedFrame
            }
        }

        guard var captureRect = unionRect else { return }
        captureRect = captureRect.intersection(sourceView.bounds)
        guard captureRect.width > 0, captureRect.height > 0 else { return }

        UIGraphicsBeginImageContextWithOptions(captureRect.size, true, scale)
        defer { UIGraphicsEndImageContext() }

        guard let ctx = UIGraphicsGetCurrentContext() else { return }

        ctx.translateBy(x: -captureRect.origin.x, y: -captureRect.origin.y)

        var savedStates: [(UIView, CGFloat)] = []
        for (view, _) in viewFrames {
            var parentGlassView: UIView? = view.superview
            while let p = parentGlassView {
                if p is GlassBackgroundView {
                    break
                }
                parentGlassView = p.superview
            }

            if let parent = parentGlassView, parent.alpha > 0 {
                savedStates.append((parent, parent.alpha))
                parent.alpha = 0
            }
        }

        sourceView.layer.render(in: ctx)

        for (view, alpha) in savedStates {
            view.alpha = alpha
        }

        guard let snapshot = UIGraphicsGetImageFromCurrentImageContext(),
              let cgImage = snapshot.cgImage else { return }

        let ciImage = CIImage(cgImage: cgImage)
        blurFilter.setValue(ciImage, forKey: kCIInputImageKey)

        guard let blurredCIImage = blurFilter.outputImage,
              let blurredCGImage = ciContext.createCGImage(blurredCIImage, from: ciImage.extent) else {
            return
        }

        let blurredImage = UIImage(cgImage: blurredCGImage, scale: scale, orientation: .up)

        for (view, frameInSource) in viewFrames {
            let offsetInCapture = CGPoint(
                x: frameInSource.origin.x - captureRect.origin.x,
                y: frameInSource.origin.y - captureRect.origin.y
            )

            view.updateWithSharedCapture(
                blurredImage: blurredImage,
                captureRect: captureRect.size,
                offsetInCapture: offsetInCapture
            )
        }
    }
}
