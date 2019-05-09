// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var ReactNative = require("react-native");
var UtilsRN$BsReactNativeWeb = require("../private/utilsRN.js");

function make(accessible, accessibilityLabel, accessibilityComponentType, accessibilityTraits, delayLongPress, delayPressIn, delayPressOut, disabled, hitSlop, onLayout, onLongPress, onPress, onPressIn, onPressOut, pressRetentionOffset, style, background, useForeground, testID) {
  var partial_arg = {
    accessible: accessible,
    accessibilityLabel: accessibilityLabel,
    delayLongPress: delayLongPress,
    delayPressIn: delayPressIn,
    delayPressOut: delayPressOut,
    disabled: disabled,
    hitSlop: hitSlop,
    onLayout: onLayout,
    onLongPress: onLongPress,
    onPress: onPress,
    background: background,
    onPressIn: onPressIn,
    onPressOut: onPressOut,
    pressRetentionOffset: pressRetentionOffset,
    style: style,
    useForeground: useForeground,
    accessibilityComponentType: UtilsRN$BsReactNativeWeb.option_map((function (x) {
            if (x >= 310731477) {
              if (x >= 735392028) {
                return "radiobutton_unchecked";
              } else {
                return "radiobutton_checked-none";
              }
            } else if (x >= -639606286) {
              return "button";
            } else {
              return "none";
            }
          }), accessibilityComponentType),
    accessibilityTraits: UtilsRN$BsReactNativeWeb.option_map((function (traits) {
            var to_string = function (param) {
              if (param >= -522290177) {
                if (param >= -50329203) {
                  if (param !== 5343647) {
                    if (param >= 829237851) {
                      if (param >= 1030299483) {
                        return "image";
                      } else {
                        return "selected";
                      }
                    } else if (param >= 15272742) {
                      return "summary";
                    } else {
                      return "header";
                    }
                  } else {
                    return "key";
                  }
                } else if (param >= -123392599) {
                  if (param >= -66829956) {
                    return "disabled";
                  } else {
                    return "adjustable";
                  }
                } else if (param >= -487088280) {
                  return "search";
                } else {
                  return "allowsDirectInteraction";
                }
              } else if (param >= -727597924) {
                if (param >= -683287213) {
                  if (param >= -639606286) {
                    return "button";
                  } else {
                    return "startsMedia";
                  }
                } else if (param >= -707934868) {
                  return "pageTurn";
                } else {
                  return "frequentUpdates";
                }
              } else if (param >= -922086728) {
                if (param >= -856044371) {
                  return "text";
                } else {
                  return "none";
                }
              } else if (param >= -944564230) {
                return "link";
              } else {
                return "plays";
              }
            };
            return $$Array.of_list(List.map(to_string, traits));
          }), accessibilityTraits),
    testID: testID
  };
  var partial_arg$1 = ReactNative.TouchableNativeFeedback;
  return (function (param) {
      return ReasonReact.wrapJsForReason(partial_arg$1, partial_arg, param);
    });
}

function selectableBackground(prim) {
  return ReactNative.TouchableNativeFeedback.SelectableBackground();
}

function selectableBackgroundBorderless(prim) {
  return ReactNative.TouchableNativeFeedback.SelectableBackgroundBorderless();
}

function canUseNativeForeground(prim) {
  return ReactNative.TouchableNativeFeedback.CanUseNativeForeground();
}

function ripple(prim, prim$1) {
  return ReactNative.TouchableNativeFeedback.Ripple(prim, prim$1);
}

exports.make = make;
exports.selectableBackground = selectableBackground;
exports.selectableBackgroundBorderless = selectableBackgroundBorderless;
exports.canUseNativeForeground = canUseNativeForeground;
exports.ripple = ripple;
/* ReasonReact Not a pure module */
