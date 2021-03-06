// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var $$Array = require("bs-platform/lib/js/array.js");
var Curry = require("bs-platform/lib/js/curry.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var ReactNative = require("react-native");
var UtilsRN$BsReactNativeWeb = require("../private/utilsRN.js");

var Event = /* module */[];

function make(onError, onLayout, onLoad, onLoadEnd, onLoadStart, resizeMode, source, style, imageStyle, testID, resizeMethod, accessibilityLabel, accessible, blurRadius, capInsets, defaultSource, onPartialLoad, onProgress) {
  var partial_arg = {
    onLayout: onLayout,
    onError: onError,
    onLoad: onLoad,
    onLoadEnd: onLoadEnd,
    onLoadStart: onLoadStart,
    resizeMode: UtilsRN$BsReactNativeWeb.option_map((function (x) {
            if (x !== -162316795) {
              if (x >= 108828507) {
                if (x >= 427065300) {
                  return "contain";
                } else {
                  return "repeat";
                }
              } else if (x >= 98248149) {
                return "center";
              } else {
                return "cover";
              }
            } else {
              return "stretch";
            }
          }), resizeMode),
    source: UtilsRN$BsReactNativeWeb.option_map((function (x) {
            if (x[0] >= 4245324) {
              return x[1];
            } else {
              return $$Array.of_list(x[1]);
            }
          }), source),
    style: style,
    imageStyle: imageStyle,
    testID: testID,
    resizeMethod: UtilsRN$BsReactNativeWeb.option_map((function (x) {
            if (x !== -120664438) {
              if (x >= 142301684) {
                return "resize";
              } else {
                return "auto";
              }
            } else {
              return "scale";
            }
          }), resizeMethod),
    accessibilityLabel: accessibilityLabel,
    accessible: accessible,
    blurRadius: blurRadius,
    capInsets: capInsets,
    defaultSource: UtilsRN$BsReactNativeWeb.option_map((function (x) {
            return x[1];
          }), defaultSource),
    onPartialLoad: onPartialLoad,
    onProgress: UtilsRN$BsReactNativeWeb.option_map((function (x, y) {
            return Curry._1(x, y.nativeEvent);
          }), onProgress)
  };
  var partial_arg$1 = ReactNative.ImageBackground;
  return (function (param) {
      return ReasonReact.wrapJsForReason(partial_arg$1, partial_arg, param);
    });
}

exports.Event = Event;
exports.make = make;
/* ReasonReact Not a pure module */
