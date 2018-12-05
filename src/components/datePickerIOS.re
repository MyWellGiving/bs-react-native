[@bs.module "react-native"]
external view: ReasonReact.reactClass = "DatePickerIOS";

let encodeMode = prop =>
  switch (prop) {
  | `date => "date"
  | `time => "time"
  | `datetime => "datetime"
  };

let make =
    (
      ~date,
      ~onDateChange,
      ~maximumDate=?,
      ~minimumDate=?,
      ~mode=?,
      ~minuteInterval=?,
      ~timeZoneOffsetInMinutes=?,
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
    ) =>
  ReasonReact.wrapJsForReason(
    ~reactClass=view,
    ~props=
      Props.extendView(
        {
          "date": date,
          "onDateChange": onDateChange,
          "maximumDate": maximumDate,
          "minimumDate": minimumDate,
          "mode": UtilsRN.option_map(encodeMode, mode),
          "minuteInterval": minuteInterval,
          "timeZoneOffsetInMinutes": timeZoneOffsetInMinutes,
        },
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
      ),
  );