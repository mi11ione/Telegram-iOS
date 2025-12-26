import Foundation
import UIKit
import AsyncDisplayKit

private final class SwitchNodeViewLayer: CALayer {
    override func setNeedsDisplay() {
    }
}

private final class SwitchNodeView: UISwitch {
    override class var layerClass: AnyClass {
        if #available(iOS 26.0, *) {
            return super.layerClass
        } else {
            return SwitchNodeViewLayer.self
        }
    }
}

public protocol CustomSwitchView: UIControl {
    var isOn: Bool { get set }
    var onTintColor: UIColor { get set }
    var offTintColor: UIColor { get set }
    var intrinsicContentSize: CGSize { get }
    func setOn(_ on: Bool, animated: Bool)
}

public struct CustomSwitchViewFactory {
    public static var createCustomSwitch: (() -> CustomSwitchView)?

    public static var useCustomSwitch: Bool {
        return createCustomSwitch != nil
    }
}

open class SwitchNode: ASDisplayNode {
    public var valueUpdated: ((Bool) -> Void)?
    
    public var frameColor = UIColor(rgb: 0xe0e0e0) {
        didSet {
            if self.isNodeLoaded {
                if oldValue != self.frameColor {
                    if let switchView = self.view as? UISwitch {
                        switchView.tintColor = self.frameColor
                    } else if let customSwitch = self.view as? CustomSwitchView {
                        customSwitch.offTintColor = self.frameColor
                    }
                }
            }
        }
    }
    public var handleColor = UIColor(rgb: 0xffffff) {
        didSet {
            if self.isNodeLoaded {
                //(self.view as! UISwitch).thumbTintColor = self.handleColor
            }
        }
    }
    public var contentColor = UIColor(rgb: 0x42d451) {
        didSet {
            if self.isNodeLoaded {
                if oldValue != self.contentColor {
                    if let switchView = self.view as? UISwitch {
                        switchView.onTintColor = self.contentColor
                    } else if let customSwitch = self.view as? CustomSwitchView {
                        customSwitch.onTintColor = self.contentColor
                    }
                }
            }
        }
    }
    
    private var _isOn: Bool = false
    public var isOn: Bool {
        get {
            return self._isOn
        } set(value) {
            if (value != self._isOn) {
                self._isOn = value
                if self.isNodeLoaded {
                    if let switchView = self.view as? UISwitch {
                        switchView.setOn(value, animated: false)
                    } else if let customSwitch = self.view as? CustomSwitchView {
                        customSwitch.setOn(value, animated: false)
                    }
                }
            }
        }
    }

    private let useCustomSwitch: Bool

    override public init() {
        self.useCustomSwitch = CustomSwitchViewFactory.useCustomSwitch
        super.init()

        if self.useCustomSwitch, let createCustomSwitch = CustomSwitchViewFactory.createCustomSwitch {
            self.setViewBlock({
                return createCustomSwitch()
            })
        } else {
            self.setViewBlock({
                return SwitchNodeView()
            })
        }
    }
    
    override open func didLoad() {
        super.didLoad()
        
        self.view.isAccessibilityElement = false

        if let switchView = self.view as? UISwitch {
            switchView.backgroundColor = self.backgroundColor
            switchView.tintColor = self.frameColor
            switchView.onTintColor = self.contentColor
            switchView.setOn(self._isOn, animated: false)
            switchView.addTarget(self, action: #selector(switchValueChanged(_:)), for: .valueChanged)
        } else if let customSwitch = self.view as? CustomSwitchView {
            customSwitch.offTintColor = self.frameColor
            customSwitch.onTintColor = self.contentColor
            customSwitch.setOn(self._isOn, animated: false)
            customSwitch.addTarget(self, action: #selector(customSwitchValueChanged(_:)), for: .valueChanged)
        }
    }
    
    public func setOn(_ value: Bool, animated: Bool) {
        self._isOn = value
        if self.isNodeLoaded {
            if let switchView = self.view as? UISwitch {
                switchView.setOn(value, animated: animated)
            } else if let customSwitch = self.view as? CustomSwitchView {
                customSwitch.setOn(value, animated: animated)
            }
        }
    }
    
    override open func calculateSizeThatFits(_ constrainedSize: CGSize) -> CGSize {
        if self.useCustomSwitch {
            return CGSize(width: 68.0, height: 30.0)
        } else if #available(iOS 26.0, *) {
            return CGSize(width: 63.0, height: 28.0)
        } else {
            return CGSize(width: 51.0, height: 31.0)
        }
    }
    
    @objc func switchValueChanged(_ view: UISwitch) {
        self._isOn = view.isOn
        self.valueUpdated?(view.isOn)
    }

    @objc func customSwitchValueChanged(_ control: UIControl) {
        if let customSwitch = control as? CustomSwitchView {
            self._isOn = customSwitch.isOn
            self.valueUpdated?(customSwitch.isOn)
        }
    }
}
