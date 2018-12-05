[@bs.deriving jsConverter]
type behavior = [ | `height | `position | `padding];

[@bs.module "react-native"]
external keyboardAvoidingView: ReasonReact.reactClass = "KeyboardAvoidingView";

let make =
    (
      ~accessibilityLabel=?,
      ~accessible=?,
      ~hitSlop=?,
      ~onAccessibilityTap=?,
      ~onLayout=?,
      ~onMagicTap=?,
      ~responderHandlers=?,
      ~pointerEvents=?,
      ~removeClippedSubviews=?,
      ~style=?,
      ~testID=?,
      ~accessibilityComponentType=?,
      ~accessibilityLiveRegion=?,
      ~collapsable=?,
      ~importantForAccessibility=?,
      ~needsOffscreenAlphaCompositing=?,
      ~renderToHardwareTextureAndroid=?,
      ~accessibilityTraits=?,
      ~accessibilityViewIsModal=?,
      ~shouldRasterizeIOS=?,
      ~keyboardVerticalOffset=?,
      ~behavior=?,
      ~contentContainerStyle=?,
    ) =>
  ReasonReact.wrapJsForReason(
    ~reactClass=keyboardAvoidingView,
    ~props=
      Props.extendView(
        ~accessibilityLabel?,
        ~accessible?,
        ~hitSlop?,
        ~onAccessibilityTap?,
        ~onLayout?,
        ~onMagicTap?,
        ~responderHandlers?,
        ~pointerEvents?,
        ~removeClippedSubviews?,
        ~style?,
        ~testID?,
        ~accessibilityComponentType?,
        ~accessibilityLiveRegion?,
        ~collapsable?,
        ~importantForAccessibility?,
        ~needsOffscreenAlphaCompositing?,
        ~renderToHardwareTextureAndroid?,
        ~accessibilityTraits?,
        ~accessibilityViewIsModal?,
        ~shouldRasterizeIOS?,
        {
          "keyboardVerticalOffset": keyboardVerticalOffset,
          "behavior": UtilsRN.option_map(behaviorToJs, behavior),
          "contentContainerStyle": contentContainerStyle,
        },
      ),
  );