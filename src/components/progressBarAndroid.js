// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE
'use strict';

var ReasonReact = require("reason-react/src/ReasonReact.js");
var ReactNative = require("react-native");
var Props$BsReactNativeWeb = require("../private/props.js");
var UtilsRN$BsReactNativeWeb = require("../private/utilsRN.js");

function styleAttribute(attr) {
  if (attr >= 188904336) {
    if (attr >= 311976103) {
      if (attr >= 912164297) {
        return "SmallInverse";
      } else {
        return "Small";
      }
    } else if (attr >= 208994564) {
      return "Horizontal";
    } else {
      return "Inverse";
    }
  } else if (attr !== -175158955) {
    if (attr >= 48800667) {
      return "Large";
    } else {
      return "Normal";
    }
  } else {
    return "LargeInverse";
  }
}

function make(animating, color, indeterminate, progress, styleAttr, accessibilityLabel, accessible, hitSlop, onAccessibilityTap, onLayout, onMagicTap, responderHandlers, pointerEvents, removeClippedSubviews, style, testID, accessibilityComponentType, accessibilityLiveRegion, collapsable, importantForAccessibility, needsOffscreenAlphaCompositing, renderToHardwareTextureAndroid, accessibilityTraits, accessibilityViewIsModal, shouldRasterizeIOS) {
  var partial_arg = Props$BsReactNativeWeb.extendView(accessibilityLabel, accessible, hitSlop, onAccessibilityTap, onLayout, onMagicTap, responderHandlers, pointerEvents, removeClippedSubviews, style, testID, accessibilityComponentType, accessibilityLiveRegion, collapsable, importantForAccessibility, needsOffscreenAlphaCompositing, renderToHardwareTextureAndroid, accessibilityTraits, accessibilityViewIsModal, shouldRasterizeIOS, {
        animating: animating,
        color: color,
        indeterminate: indeterminate,
        progress: progress,
        styleAttr: UtilsRN$BsReactNativeWeb.option_map(styleAttribute, styleAttr),
        testID: testID
      });
  var partial_arg$1 = ReactNative.ProgressBarAndroid;
  return (function (param) {
      return ReasonReact.wrapJsForReason(partial_arg$1, partial_arg, param);
    });
}

exports.make = make;
/* ReasonReact Not a pure module */
