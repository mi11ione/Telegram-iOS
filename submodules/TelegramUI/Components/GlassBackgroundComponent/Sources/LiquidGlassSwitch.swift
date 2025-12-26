import UIKit
import Display

public final class LiquidGlassSwitch: UIControl, CustomSwitchView {
    public var isOn: Bool = false {
        didSet {
            if oldValue != isOn {
                updateThumbPosition(animated: true)
                sendActions(for: .valueChanged)
            }
        }
    }

    public var onTintColor: UIColor = UIColor.systemGreen {
        didSet { updateColors() }
    }

    public var offTintColor: UIColor = UIColor(white: 0.9, alpha: 1.0) {
        didSet { updateColors() }
    }

    private let trackView: UIView = {
        let view = UIView()
        view.layer.cornerRadius = 15
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

    private let trackSize = CGSize(width: 68, height: 30)
    private let thumbSize = CGSize(width: 42, height: 27)
    private let thumbPadding: CGFloat = 2
    private var isDragging = false
    private var dragStartX: CGFloat = 0
    private var dragStartOn: Bool = false

    public override init(frame: CGRect) {
        super.init(frame: frame)
        setup()
    }

    required init?(coder: NSCoder) {
        super.init(coder: coder)
        setup()
    }

    private func setup() {
        addSubview(trackView)

        addSubview(thumbView)

        let glass = LiquidToggleGlassView()
        glass.isUserInteractionEnabled = false
        glass.alpha = 0
        addSubview(glass)
        self.glassView = glass

        updateColors()
        updateThumbPosition(animated: false)

        let tap = UITapGestureRecognizer(target: self, action: #selector(handleTap))
        addGestureRecognizer(tap)

        let pan = UIPanGestureRecognizer(target: self, action: #selector(handlePan))
        addGestureRecognizer(pan)
    }

    public override func didMoveToWindow() {
        super.didMoveToWindow()
        if let window = window {
            glassView?.setSourceView(window)
        }
    }

    public override var intrinsicContentSize: CGSize {
        return trackSize
    }

    public override func layoutSubviews() {
        super.layoutSubviews()

        trackView.frame = CGRect(
            x: (bounds.width - trackSize.width) / 2,
            y: (bounds.height - trackSize.height) / 2,
            width: trackSize.width,
            height: trackSize.height
        )

        updateThumbPosition(animated: false)
    }

    private func thumbFrame(for on: Bool) -> CGRect {
        let trackFrame = trackView.frame
        let thumbY = trackFrame.midY - thumbSize.height / 2
        let thumbX: CGFloat

        if on {
            thumbX = trackFrame.maxX - thumbPadding - thumbSize.width
        } else {
            thumbX = trackFrame.minX + thumbPadding
        }

        return CGRect(x: thumbX, y: thumbY, width: thumbSize.width, height: thumbSize.height)
    }

    private func updateThumbPosition(animated: Bool) {
        let targetFrame = thumbFrame(for: isOn)

        let update = {
            self.thumbView.frame = targetFrame
            self.thumbView.layer.cornerRadius = targetFrame.height / 2
            self.glassView?.setBaseFrame(targetFrame)
            self.updateColors()
        }

        if animated {
            UIView.animate(
                withDuration: 0.25,
                delay: 0,
                usingSpringWithDamping: 0.7,
                initialSpringVelocity: 0,
                options: [.beginFromCurrentState],
                animations: update
            )
        } else {
            update()
        }
    }

    private func updateColors() {
        let color = isOn ? onTintColor : offTintColor
        trackView.backgroundColor = color
    }

    private func blendColors(_ color1: UIColor, _ color2: UIColor, progress: CGFloat) -> UIColor {
        var r1: CGFloat = 0, g1: CGFloat = 0, b1: CGFloat = 0, a1: CGFloat = 0
        var r2: CGFloat = 0, g2: CGFloat = 0, b2: CGFloat = 0, a2: CGFloat = 0

        color1.getRed(&r1, green: &g1, blue: &b1, alpha: &a1)
        color2.getRed(&r2, green: &g2, blue: &b2, alpha: &a2)

        let p = max(0, min(1, progress))
        return UIColor(
            red: r1 + (r2 - r1) * p,
            green: g1 + (g2 - g1) * p,
            blue: b1 + (b2 - b1) * p,
            alpha: a1 + (a2 - a1) * p
        )
    }

    private var isGlassActive: Bool = false

    public override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        super.touchesBegan(touches, with: event)
        if !isTapAnimating {
            activateGlass()
        }
    }

    public override func touchesEnded(_ touches: Set<UITouch>, with event: UIEvent?) {
        super.touchesEnded(touches, with: event)
        if !isDragging && !isTapAnimating {
            deactivateGlass()
        }
    }

    public override func touchesCancelled(_ touches: Set<UITouch>, with event: UIEvent?) {
        super.touchesCancelled(touches, with: event)
        if !isDragging && !isTapAnimating {
            deactivateGlass()
        }
    }

    private func activateGlass() {
        isGlassActive = true
        glassView?.setActive(true)
        thumbView.alpha = 0
    }

    private func deactivateGlass() {
        isGlassActive = false
        glassView?.setActive(false)
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.15) { [weak self] in
            guard let self = self, !self.isGlassActive else { return }
            UIView.animate(withDuration: 0.15) {
                self.thumbView.alpha = 1
            }
        }
    }

    @objc private func handleTap(_ gesture: UITapGestureRecognizer) {
        guard !isTapAnimating else { return }
        isTapAnimating = true

        activateGlass()

        DispatchQueue.main.asyncAfter(deadline: .now() + 0.15) { [weak self] in
            guard let self = self else { return }

            self.isOn.toggle()

            DispatchQueue.main.asyncAfter(deadline: .now() + 0.15) { [weak self] in
                guard let self = self else { return }
                self.deactivateGlass()
                self.isTapAnimating = false
            }
        }
    }

    private var isTapAnimating: Bool = false

    @objc private func handlePan(_ gesture: UIPanGestureRecognizer) {
        let location = gesture.location(in: self)
        let trackFrame = trackView.frame

        switch gesture.state {
        case .began:
            isDragging = true
            dragStartX = location.x
            dragStartOn = isOn

        case .changed:
            let minX = trackFrame.minX + thumbPadding
            let maxX = trackFrame.maxX - thumbPadding - thumbSize.width

            let thumbCenterOffset = thumbSize.width / 2
            let targetX = location.x - thumbCenterOffset
            let clampedX = max(minX, min(maxX, targetX))

            let newThumbFrame = CGRect(
                x: clampedX,
                y: trackFrame.midY - thumbSize.height / 2,
                width: thumbSize.width,
                height: thumbSize.height
            )
            thumbView.frame = newThumbFrame
            glassView?.setBaseFrame(newThumbFrame)

            let progress = (clampedX - minX) / (maxX - minX)
            trackView.backgroundColor = blendColors(offTintColor, onTintColor, progress: progress)

        case .ended, .cancelled:
            isDragging = false

            let trackFrame = trackView.frame
            let midPoint = trackFrame.midX - thumbSize.width / 2
            let shouldBeOn = thumbView.frame.origin.x > midPoint

            if shouldBeOn != isOn {
                isOn = shouldBeOn
            } else {
                updateThumbPosition(animated: true)
            }

            deactivateGlass()

        default:
            break
        }
    }

    public func setOn(_ on: Bool, animated: Bool) {
        guard on != isOn else { return }

        if animated {
            isOn = on
        } else {
            let wasOn = isOn
            isOn = on
            if wasOn != on {
                updateThumbPosition(animated: false)
            }
        }
    }
}
