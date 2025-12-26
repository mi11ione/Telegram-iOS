import UIKit

public final class LiquidGlassSlider: UIControl {
    public var value: CGFloat = 0.5 {
        didSet {
            let clamped = max(minimumValue, min(maximumValue, value))
            if clamped != value {
                value = clamped
            }
            if oldValue != value {
                updateThumbPosition(animated: false)
                sendActions(for: .valueChanged)
            }
        }
    }

    public var minimumValue: CGFloat = 0.0
    public var maximumValue: CGFloat = 1.0

    public var trackTintColor: UIColor = UIColor(red: 0.0, green: 0.48, blue: 1.0, alpha: 1.0) {
        didSet { updateColors() }
    }

    public var trackBackgroundColor: UIColor = UIColor(white: 0.85, alpha: 1.0) {
        didSet { updateColors() }
    }

    public var isTrackingUpdated: ((Bool) -> Void)?

    private let trackBackgroundView: UIView = {
        let view = UIView()
        view.isUserInteractionEnabled = false
        return view
    }()

    private let trackFillView: UIView = {
        let view = UIView()
        view.isUserInteractionEnabled = false
        return view
    }()

    private let thumbView: UIView = {
        let view = UIView()
        view.backgroundColor = .white
        view.layer.shadowColor = UIColor.black.cgColor
        view.layer.shadowOffset = CGSize(width: 0, height: 2)
        view.layer.shadowRadius = 4
        view.layer.shadowOpacity = 0.15
        view.isUserInteractionEnabled = false
        return view
    }()

    private var glassView: LiquidToggleGlassView?

    private let trackHeight: CGFloat = 4
    private let thumbSize = CGSize(width: 29, height: 21)

    private var isDragging = false
    private var isGlassActive = false

    public override init(frame: CGRect) {
        super.init(frame: frame)
        setup()
    }

    required init?(coder: NSCoder) {
        super.init(coder: coder)
        setup()
    }

    private func setup() {
        addSubview(trackBackgroundView)

        addSubview(trackFillView)

        addSubview(thumbView)

        let glass = LiquidToggleGlassView()
        glass.isUserInteractionEnabled = false
        glass.alpha = 0
        glass.minificationStrength = 0
        glass.activeExpansionH = 10.0
        glass.activeExpansionV = 5.0
        glass.glassTintStrength = 0.05
        glass.refractionStrength = 0.4
        glass.chromeStrength = 4.5
        glass.edgeBandMultiplier = 0.4
        glass.instantPositionTracking = true
        glass.enableLiquidStretch = true
        addSubview(glass)
        self.glassView = glass

        updateColors()

        let pan = UIPanGestureRecognizer(target: self, action: #selector(handlePan))
        addGestureRecognizer(pan)

        let tap = UITapGestureRecognizer(target: self, action: #selector(handleTap))
        addGestureRecognizer(tap)
    }

    public override func didMoveToWindow() {
        super.didMoveToWindow()
        if let window = window {
            glassView?.setSourceView(window)
        }
    }

    public override var intrinsicContentSize: CGSize {
        return CGSize(width: 200, height: 44)
    }

    public override func layoutSubviews() {
        super.layoutSubviews()

        let trackY = (bounds.height - trackHeight) / 2

        let trackInset = thumbSize.width / 2
        trackBackgroundView.frame = CGRect(
            x: trackInset,
            y: trackY,
            width: bounds.width - trackInset * 2,
            height: trackHeight
        )
        trackBackgroundView.layer.cornerRadius = trackHeight / 2

        updateThumbPosition(animated: false)
    }

    private func normalizedValue() -> CGFloat {
        let range = maximumValue - minimumValue
        guard range > 0 else { return 0 }
        return (value - minimumValue) / range
    }

    private func thumbFrame(for normalizedValue: CGFloat) -> CGRect {
        let trackInset = thumbSize.width / 2
        let trackWidth = bounds.width - trackInset * 2
        let thumbX = trackInset + trackWidth * normalizedValue - thumbSize.width / 2
        let thumbY = (bounds.height - thumbSize.height) / 2

        return CGRect(x: thumbX, y: thumbY, width: thumbSize.width, height: thumbSize.height)
    }

    private func updateThumbPosition(animated: Bool) {
        let targetFrame = thumbFrame(for: normalizedValue())

        let update = {
            self.thumbView.frame = targetFrame
            self.thumbView.layer.cornerRadius = targetFrame.height / 2
            self.glassView?.setBaseFrame(targetFrame)

            let trackInset = self.thumbSize.width / 2
            let fillWidth = targetFrame.midX - trackInset
            if fillWidth < 2 {
                self.trackFillView.isHidden = true
            } else {
                self.trackFillView.isHidden = false
                self.trackFillView.frame = CGRect(
                    x: trackInset,
                    y: self.trackBackgroundView.frame.origin.y,
                    width: fillWidth,
                    height: self.trackHeight
                )
                self.trackFillView.layer.cornerRadius = self.trackHeight / 2
            }
        }

        if animated {
            UIView.animate(
                withDuration: 0.2,
                delay: 0,
                usingSpringWithDamping: 0.8,
                initialSpringVelocity: 0,
                options: [.beginFromCurrentState],
                animations: update
            )
        } else {
            update()
        }
    }

    private func updateColors() {
        trackBackgroundView.backgroundColor = trackBackgroundColor
        trackFillView.backgroundColor = trackTintColor
    }

    private func activateGlass() {
        guard !isGlassActive else { return }
        isGlassActive = true
        glassView?.setActive(true)
        thumbView.alpha = 0
    }

    private func deactivateGlass() {
        guard isGlassActive else { return }
        isGlassActive = false
        glassView?.setActive(false)

        DispatchQueue.main.asyncAfter(deadline: .now() + 0.15) { [weak self] in
            guard let self = self, !self.isGlassActive else { return }
            UIView.animate(withDuration: 0.15) {
                self.thumbView.alpha = 1
            }
        }
    }

    public override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        super.touchesBegan(touches, with: event)
        activateGlass()
    }

    public override func touchesEnded(_ touches: Set<UITouch>, with event: UIEvent?) {
        super.touchesEnded(touches, with: event)
        if !isDragging {
            deactivateGlass()
        }
    }

    public override func touchesCancelled(_ touches: Set<UITouch>, with event: UIEvent?) {
        super.touchesCancelled(touches, with: event)
        if !isDragging {
            deactivateGlass()
        }
    }

    @objc private func handleTap(_ gesture: UITapGestureRecognizer) {
        let location = gesture.location(in: self)
        let newValue = valueForLocation(location)

        activateGlass()

        UIView.animate(withDuration: 0.2) {
            self.value = newValue
        }

        DispatchQueue.main.asyncAfter(deadline: .now() + 0.25) { [weak self] in
            self?.deactivateGlass()
        }
    }

    @objc private func handlePan(_ gesture: UIPanGestureRecognizer) {
        let location = gesture.location(in: self)

        switch gesture.state {
        case .began:
            isDragging = true
            isTrackingUpdated?(true)

        case .changed:
            value = valueForLocation(location)

            let trackInset = thumbSize.width / 2
            let trackWidth = bounds.width - trackInset * 2
            let relativeX = location.x - trackInset
            let overdrag: CGFloat
            if relativeX < 0 {
                let distance = abs(relativeX)
                let normalized = min(1.0, pow(distance / 250.0, 0.7))
                overdrag = -normalized
            } else if relativeX > trackWidth {
                let distance = relativeX - trackWidth
                let normalized = min(1.0, pow(distance / 250.0, 0.7))
                overdrag = normalized
            } else {
                overdrag = 0
            }
            glassView?.setOverdrag(overdrag)

            let centerY = bounds.height / 2
            let verticalOffset = location.y - centerY
            let distance = abs(verticalOffset)
            let normalized = min(1.0, pow(distance / 200.0, 0.7))
            let verticalDrag = verticalOffset >= 0 ? normalized : -normalized
            glassView?.setVerticalDrag(verticalDrag)

        case .ended, .cancelled:
            isDragging = false
            isTrackingUpdated?(false)
            glassView?.setOverdrag(0)
            glassView?.setVerticalDrag(0)
            deactivateGlass()

        default:
            break
        }
    }

    private func valueForLocation(_ location: CGPoint) -> CGFloat {
        let trackInset = thumbSize.width / 2
        let trackWidth = bounds.width - trackInset * 2
        let relativeX = location.x - trackInset
        let normalizedValue = max(0, min(1, relativeX / trackWidth))
        return minimumValue + normalizedValue * (maximumValue - minimumValue)
    }

    public func setValue(_ newValue: CGFloat, animated: Bool) {
        let clamped = max(minimumValue, min(maximumValue, newValue))
        if animated {
            UIView.animate(withDuration: 0.2) {
                self.value = clamped
            }
        } else {
            self.value = clamped
        }
    }
}
