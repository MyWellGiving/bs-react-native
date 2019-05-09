// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE
'use strict';

var Js_primitive = require("bs-platform/lib/js/js_primitive.js");
var ReactNative = require("react-native");

function canOpenURL(url) {
  return ReactNative.Linking.canOpenURL(url).then((function (bool) {
                return Promise.resolve(bool);
              }));
}

function getInitialURL(param) {
  return ReactNative.Linking.getInitialURL().then((function (stringOrNull) {
                return Promise.resolve(stringOrNull === null ? undefined : Js_primitive.some(stringOrNull));
              }));
}

exports.canOpenURL = canOpenURL;
exports.getInitialURL = getInitialURL;
/* react-native Not a pure module */
