// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var ReasonReact = require("reason-react/src/ReasonReact.js");
var ReactNative = require("react-native");
var Props$BsReactNativeWeb = require("../private/props.js");
var UtilsRN$BsReactNativeWeb = require("../private/utilsRN.js");

function encodeMode(prop) {
  if (prop !== -855847923) {
    if (prop >= -281947845) {
      return "datetime";
    } else {
      return "date";
    }
  } else {
    return "time";
  }
}

function make(date, onDateChange, maximumDate, minimumDate, mode, minuteInterval, timeZoneOffsetInMinutes, accessibilityLabel, accessible, hitSlop, onAccessibilityTap, onLayout, onMagicTap, responderHandlers, pointerEvents, removeClippedSubviews, style, testID, accessibilityComponentType, accessibilityLiveRegion, collapsable, importantForAccessibility, needsOffscreenAlphaCompositing, renderToHardwareTextureAndroid, accessibilityTraits, accessibilityViewIsModal, shouldRasterizeIOS) {
  var partial_arg = Props$BsReactNativeWeb.extendView(accessibilityLabel, accessible, hitSlop, onAccessibilityTap, onLayout, onMagicTap, responderHandlers, pointerEvents, removeClippedSubviews, style, testID, accessibilityComponentType, accessibilityLiveRegion, collapsable, importantForAccessibility, needsOffscreenAlphaCompositing, renderToHardwareTextureAndroid, accessibilityTraits, accessibilityViewIsModal, shouldRasterizeIOS, {
        date: date,
        onDateChange: onDateChange,
        maximumDate: maximumDate,
        minimumDate: minimumDate,
        mode: UtilsRN$BsReactNativeWeb.option_map(encodeMode, mode),
        minuteInterval: minuteInterval,
        timeZoneOffsetInMinutes: timeZoneOffsetInMinutes
      });
  var partial_arg$1 = ReactNative.DatePickerIOS;
  return (function (param) {
      return ReasonReact.wrapJsForReason(partial_arg$1, partial_arg, param);
    });
}

exports.make = make;
/* ReasonReact Not a pure module */