// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var $$Array = require("bs-platform/lib/js/array.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var ReactNative = require("react-native");
var Props$BsReactNativeWeb = require("../private/props.js");
var UtilsRN$BsReactNativeWeb = require("../private/utilsRN.js");

function convertImageSource(src) {
  if (src[0] >= 4245324) {
    return src[1];
  } else {
    return $$Array.of_list(src[1]);
  }
}

function make(disabled, maximumTrackTintColor, maximumValue, minimumTrackTintColor, minimumValue, onSlidingComplete, onValueChange, step, value, thumbTintColor, maximumTrackImage, minimumTrackImage, thumbImage, trackImage, accessibilityLabel, accessible, hitSlop, onAccessibilityTap, onLayout, onMagicTap, responderHandlers, pointerEvents, removeClippedSubviews, style, testID, accessibilityComponentType, accessibilityLiveRegion, collapsable, importantForAccessibility, needsOffscreenAlphaCompositing, renderToHardwareTextureAndroid, accessibilityTraits, accessibilityViewIsModal, shouldRasterizeIOS) {
  var partial_arg = Props$BsReactNativeWeb.extendView(accessibilityLabel, accessible, hitSlop, onAccessibilityTap, onLayout, onMagicTap, responderHandlers, pointerEvents, removeClippedSubviews, style, testID, accessibilityComponentType, accessibilityLiveRegion, collapsable, importantForAccessibility, needsOffscreenAlphaCompositing, renderToHardwareTextureAndroid, accessibilityTraits, accessibilityViewIsModal, shouldRasterizeIOS, {
        disabled: disabled,
        maximumTrackTintColor: maximumTrackTintColor,
        maximumValue: maximumValue,
        minimumTrackTintColor: minimumTrackTintColor,
        minimumValue: minimumValue,
        onSlidingComplete: onSlidingComplete,
        onValueChange: onValueChange,
        step: step,
        value: value,
        thumbTintColor: thumbTintColor,
        maximumTrackImage: UtilsRN$BsReactNativeWeb.option_map(convertImageSource, maximumTrackImage),
        minimumTrackImage: UtilsRN$BsReactNativeWeb.option_map(convertImageSource, minimumTrackImage),
        thumbImage: UtilsRN$BsReactNativeWeb.option_map(convertImageSource, thumbImage),
        trackImage: UtilsRN$BsReactNativeWeb.option_map(convertImageSource, trackImage)
      });
  var partial_arg$1 = ReactNative.Slider;
  return (function (param) {
      return ReasonReact.wrapJsForReason(partial_arg$1, partial_arg, param);
    });
}

exports.make = make;
/* ReasonReact Not a pure module */