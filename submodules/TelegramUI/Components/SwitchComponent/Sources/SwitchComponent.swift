import Foundation
import UIKit
import Display
import AsyncDisplayKit
import ComponentFlow
import TelegramPresentationData
import GlassBackgroundComponent

public final class SwitchComponent: Component {
    public typealias EnvironmentType = Empty
    
    let tintColor: UIColor?
    let value: Bool
    let valueUpdated: (Bool) -> Void
    
    public init(
        tintColor: UIColor? = nil,
        value: Bool,
        valueUpdated: @escaping (Bool) -> Void
    ) {
        self.tintColor = tintColor
        self.value = value
        self.valueUpdated = valueUpdated
    }
    
    public static func ==(lhs: SwitchComponent, rhs: SwitchComponent) -> Bool {
        if lhs.tintColor != rhs.tintColor {
            return false
        }
        if lhs.value != rhs.value {
            return false
        }
        return true
    }
    
    public final class View: UIView {
        private var nativeSwitchView: UISwitch?
        private var liquidGlassSwitch: LiquidGlassSwitch?

        private var component: SwitchComponent?
        
        override init(frame: CGRect) {
            super.init(frame: frame)
        }
        
        required init?(coder: NSCoder) {
            fatalError("init(coder:) has not been implemented")
        }
        
        @objc func valueChanged(_ sender: Any) {
            if let nativeSwitchView = self.nativeSwitchView {
                self.component?.valueUpdated(nativeSwitchView.isOn)
            } else if let liquidGlassSwitch = self.liquidGlassSwitch {
                self.component?.valueUpdated(liquidGlassSwitch.isOn)
            }
        }
        
        func update(component: SwitchComponent, availableSize: CGSize, state: EmptyComponentState, environment: Environment<EnvironmentType>, transition: ComponentTransition) -> CGSize {
            self.component = component

            if #unavailable(iOS 26.0) {
                if let nativeSwitchView = self.nativeSwitchView {
                    nativeSwitchView.removeFromSuperview()
                    self.nativeSwitchView = nil
                }

                let liquidSwitch: LiquidGlassSwitch
                if let existing = self.liquidGlassSwitch {
                    liquidSwitch = existing
                } else {
                    liquidSwitch = LiquidGlassSwitch()
                    liquidSwitch.addTarget(self, action: #selector(self.valueChanged(_:)), for: .valueChanged)
                    self.addSubview(liquidSwitch)
                    self.liquidGlassSwitch = liquidSwitch
                }

                if let tintColor = component.tintColor {
                    liquidSwitch.onTintColor = tintColor
                }
                liquidSwitch.setOn(component.value, animated: !transition.animation.isImmediate)

                let size = liquidSwitch.intrinsicContentSize
                liquidSwitch.frame = CGRect(origin: .zero, size: size)

                return size
            } else {
                if let liquidGlassSwitch = self.liquidGlassSwitch {
                    liquidGlassSwitch.removeFromSuperview()
                    self.liquidGlassSwitch = nil
                }

                let switchView: UISwitch
                if let existing = self.nativeSwitchView {
                    switchView = existing
                } else {
                    switchView = UISwitch()
                    switchView.addTarget(self, action: #selector(self.valueChanged(_:)), for: .valueChanged)
                    self.addSubview(switchView)
                    self.nativeSwitchView = switchView
                }

                switchView.tintColor = component.tintColor
                switchView.setOn(component.value, animated: !transition.animation.isImmediate)

                switchView.sizeToFit()
                switchView.frame = CGRect(origin: .zero, size: switchView.frame.size)

                return switchView.frame.size
            }
        }
    }

    public func makeView() -> View {
        return View(frame: CGRect())
    }
    
    public func update(view: View, availableSize: CGSize, state: EmptyComponentState, environment: Environment<EnvironmentType>, transition: ComponentTransition) -> CGSize {
        return view.update(component: self, availableSize: availableSize, state: state, environment: environment, transition: transition)
    }
}
