// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var ReasonReact = require("reason-react/src/ReasonReact.js");
var ReactNative = require("react-native");
var Props$BsReactNativeWeb = require("../private/props.js");
var UtilsRN$BsReactNativeWeb = require("../private/utilsRN.js");

function make(selected, badge, icon, onPress, renderAsOriginal, badgeColor, selectedIcon, style, systemIcon, title, isTVSelectable, accessibilityLabel, accessible, hitSlop, onAccessibilityTap, onLayout, onMagicTap, responderHandlers, pointerEvents, removeClippedSubviews, testID, accessibilityComponentType, accessibilityLiveRegion, collapsable, importantForAccessibility, needsOffscreenAlphaCompositing, renderToHardwareTextureAndroid, accessibilityTraits, accessibilityViewIsModal, shouldRasterizeIOS) {
  var partial_arg = Props$BsReactNativeWeb.extendView(accessibilityLabel, accessible, hitSlop, onAccessibilityTap, onLayout, onMagicTap, responderHandlers, pointerEvents, removeClippedSubviews, style, testID, accessibilityComponentType, accessibilityLiveRegion, collapsable, importantForAccessibility, needsOffscreenAlphaCompositing, renderToHardwareTextureAndroid, accessibilityTraits, accessibilityViewIsModal, shouldRasterizeIOS, {
        selected: selected,
        badge: badge,
        icon: icon,
        onPress: onPress,
        renderAsOriginal: renderAsOriginal,
        badgeColor: badgeColor,
        selectedIcon: selectedIcon,
        style: style,
        systemIcon: UtilsRN$BsReactNativeWeb.option_map((function (x) {
                if (x >= 306050430) {
                  if (x >= 560120362) {
                    if (x >= 745984467) {
                      if (x >= 838090415) {
                        return "top-rated";
                      } else {
                        return "contacts";
                      }
                    } else if (x >= 710683512) {
                      return "recents";
                    } else {
                      return "favourites";
                    }
                  } else if (x >= 316108014) {
                    return "featured";
                  } else {
                    return "most-recent";
                  }
                } else if (x >= -487088280) {
                  if (x >= -89798937) {
                    if (x >= -69751829) {
                      return "downloads";
                    } else {
                      return "most-viewed";
                    }
                  } else if (x >= -465095340) {
                    return "history";
                  } else {
                    return "search";
                  }
                } else if (x >= -529952003) {
                  return "fill";
                } else {
                  return "more";
                }
              }), systemIcon),
        title: title,
        isTVSelectable: isTVSelectable
      });
  var partial_arg$1 = ReactNative.TabBarIOS.Item;
  return (function (param) {
      return ReasonReact.wrapJsForReason(partial_arg$1, partial_arg, param);
    });
}

var Item = /* module */[/* make */make];

function make$1(barStyle, barTintColor, itemPositioning, tintColor, translucent, unselectedItemTintColor, unselectedTintColor, accessibilityLabel, accessible, hitSlop, onAccessibilityTap, onLayout, onMagicTap, responderHandlers, pointerEvents, removeClippedSubviews, testID, style, accessibilityComponentType, accessibilityLiveRegion, collapsable, importantForAccessibility, needsOffscreenAlphaCompositing, renderToHardwareTextureAndroid, accessibilityTraits, accessibilityViewIsModal, shouldRasterizeIOS) {
  var partial_arg = Props$BsReactNativeWeb.extendView(accessibilityLabel, accessible, hitSlop, onAccessibilityTap, onLayout, onMagicTap, responderHandlers, pointerEvents, removeClippedSubviews, style, testID, accessibilityComponentType, accessibilityLiveRegion, collapsable, importantForAccessibility, needsOffscreenAlphaCompositing, renderToHardwareTextureAndroid, accessibilityTraits, accessibilityViewIsModal, shouldRasterizeIOS, {
        barStyle: UtilsRN$BsReactNativeWeb.option_map((function (x) {
                if (x >= 888264127) {
                  return "black";
                } else {
                  return "default";
                }
              }), barStyle),
        barTintColor: barTintColor,
        itemPositioning: UtilsRN$BsReactNativeWeb.option_map((function (x) {
                if (x !== -1011102077) {
                  if (x >= 98248149) {
                    return "center";
                  } else {
                    return "auto";
                  }
                } else {
                  return "fill";
                }
              }), itemPositioning),
        tintColor: tintColor,
        translucent: translucent,
        unselectedItemTintColor: unselectedItemTintColor,
        unselectedTintColor: unselectedTintColor
      });
  var partial_arg$1 = ReactNative.TabBarIOS;
  return (function (param) {
      return ReasonReact.wrapJsForReason(partial_arg$1, partial_arg, param);
    });
}

exports.make = make$1;
exports.Item = Item;
/* ReasonReact Not a pure module */