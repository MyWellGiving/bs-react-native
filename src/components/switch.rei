/**
A component that renders [Switch] component usage in official docs: {{:https://facebook.github.io/react-native/docs/switch}}

This is a controlled component that requires an [onValueChange] callback that updates the [value] prop in order for the component to reflect user actions. If the [value] prop is not updated, the component will continue to render the supplied [value] prop instead of the expected result of any user actions.

  {3 Props}

  {{:\BsReactNative/View-BsReactNative} [View] props}

  {4 disabled}
  {[
    ~disabled: bool=?
  ]}
  {4 onTintColor}
  {[
    ~onTintColor: string=?
  ]}
  {4 onValueChange}
  {[
    ~onValueChange: bool => unit=?
  ]}
  {4 thumbTintColor}
  {[
    ~thumbTintColor: string=?
  ]}
  {4 tintColor}
  {[
    ~tintColor: string=?
  ]}
  {4 value}
  {[
    ~value: bool=?
  ]}
 */

let make:
  (
    ~disabled: bool=?,
    ~onTintColor: string=?,
    ~onValueChange: bool => unit=?,
    ~thumbTintColor: string=?,
    ~tintColor: string=?,
    ~value: bool=?,
    ~accessibilityLabel: ReasonReact.reactElement=?,
    ~accessible: bool=?,
    ~hitSlop: Types.insets=?,
    ~onAccessibilityTap: unit => unit=?,
    ~onLayout: RNEvent.NativeLayoutEvent.t => unit=?,
    ~onMagicTap: unit => unit=?,
    ~responderHandlers: Types.touchResponderHandlers=?,
    ~pointerEvents: [ | `auto | `none | `boxNone | `boxOnly]=?,
    ~removeClippedSubviews: bool=?,
    ~style: Style.t=?,
    ~testID: string=?,
    ~accessibilityComponentType: [
                                   | `none
                                   | `button
                                   | `radiobutton_checked
                                   | `radiobutton_unchecked
                                 ]
                                   =?,
    ~accessibilityLiveRegion: [ | `none | `polite | `assertive]=?,
    ~collapsable: bool=?,
    ~importantForAccessibility: [ | `auto | `yes | `no | `noHideDescendants]=?,
    ~needsOffscreenAlphaCompositing: bool=?,
    ~renderToHardwareTextureAndroid: bool=?,
    ~accessibilityTraits: list(
                            [
                              | `none
                              | `button
                              | `link
                              | `header
                              | `search
                              | `image
                              | `selected
                              | `plays
                              | `key
                              | `text
                              | `summary
                              | `disabled
                              | `frequentUpdates
                              | `startsMedia
                              | `adjustable
                              | `allowsDirectInteraction
                              | `pageTurn
                            ],
                          )
                            =?,
    ~accessibilityViewIsModal: bool=?,
    ~shouldRasterizeIOS: bool=?,
    array(ReasonReact.reactElement)
  ) =>
  ReasonReact.component(
    ReasonReact.stateless,
    ReasonReact.noRetainedProps,
    unit,
  );
