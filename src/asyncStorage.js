// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE
'use strict';

var $$Array = require("bs-platform/lib/js/array.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Js_primitive = require("bs-platform/lib/js/js_primitive.js");
var ReactNative = require("react-native");

function toOption(x) {
  return Promise.resolve((x == null) ? undefined : Js_primitive.some(x));
}

function map_cb(cb, err, value) {
  return Curry._2(cb, err === null ? undefined : Js_primitive.some(err), (value == null) ? undefined : Js_primitive.some(value));
}

function getItem(key, callback, param) {
  if (callback !== undefined) {
    var cb = callback;
    return ReactNative.AsyncStorage.getItem(key, (function (param, param$1) {
                    return map_cb(cb, param, param$1);
                  })).then(toOption);
  } else {
    return ReactNative.AsyncStorage.getItem(key, undefined).then(toOption);
  }
}

function setItem(key, value, callback, param) {
  if (callback !== undefined) {
    var cb = callback;
    return ReactNative.AsyncStorage.setItem(key, value, (function (param) {
                  return Curry._1(cb, param === null ? undefined : Js_primitive.some(param));
                }));
  } else {
    return ReactNative.AsyncStorage.setItem(key, value, undefined);
  }
}

function removeItem(key, callback, param) {
  if (callback !== undefined) {
    var cb = callback;
    return ReactNative.AsyncStorage.removeItem(key, (function (param) {
                  return Curry._1(cb, param === null ? undefined : Js_primitive.some(param));
                }));
  } else {
    return ReactNative.AsyncStorage.removeItem(key, undefined);
  }
}

function mergeItem(key, value, callback, param) {
  if (callback !== undefined) {
    var cb = callback;
    return ReactNative.AsyncStorage.mergeItem(key, value, (function (param) {
                  return Curry._1(cb, param === null ? undefined : Js_primitive.some(param));
                }));
  } else {
    return ReactNative.AsyncStorage.mergeItem(key, value, undefined);
  }
}

function clear(callback, param) {
  if (callback !== undefined) {
    var cb = callback;
    return ReactNative.AsyncStorage.clear((function (param) {
                  return Curry._1(cb, param === null ? undefined : Js_primitive.some(param));
                }));
  } else {
    return ReactNative.AsyncStorage.clear(undefined);
  }
}

function getAllKeys(callback, param) {
  if (callback !== undefined) {
    var cb = callback;
    return ReactNative.AsyncStorage.getAllKeys((function (param, param$1) {
                    return map_cb(cb, param, param$1);
                  })).then(toOption);
  } else {
    return ReactNative.AsyncStorage.getAllKeys(undefined).then(toOption);
  }
}

function multiGet(keys, callback, param) {
  if (callback !== undefined) {
    var cb = callback;
    return ReactNative.AsyncStorage.multiGet(keys, (function (param, param$1) {
                    return map_cb(cb, param, param$1);
                  })).then(toOption);
  } else {
    return ReactNative.AsyncStorage.multiGet(keys, undefined).then(toOption);
  }
}

function multiSet(keyValues, callback, param) {
  var combine = function (param) {
    return /* array */[
            param[0],
            param[1]
          ];
  };
  if (callback !== undefined) {
    var cb = callback;
    return ReactNative.AsyncStorage.multiSet($$Array.map(combine, keyValues), (function (param) {
                  return Curry._1(cb, param === null ? undefined : Js_primitive.some(param));
                }));
  } else {
    return ReactNative.AsyncStorage.multiSet($$Array.map(combine, keyValues), undefined);
  }
}

function multiRemove(keys, callback, param) {
  if (callback !== undefined) {
    var cb = callback;
    return ReactNative.AsyncStorage.multiRemove(keys, (function (param) {
                  return Curry._1(cb, param === null ? undefined : Js_primitive.some(param));
                }));
  } else {
    return ReactNative.AsyncStorage.multiRemove(keys, undefined);
  }
}

function multiMerge(keyValues, callback, param) {
  var combine = function (param) {
    return /* array */[
            param[0],
            param[1]
          ];
  };
  if (callback !== undefined) {
    var cb = callback;
    return ReactNative.AsyncStorage.multiMerge($$Array.map(combine, keyValues), (function (param) {
                  return Curry._1(cb, param === null ? undefined : Js_primitive.some(param));
                }));
  } else {
    return ReactNative.AsyncStorage.multiMerge($$Array.map(combine, keyValues), undefined);
  }
}

exports.getItem = getItem;
exports.setItem = setItem;
exports.removeItem = removeItem;
exports.mergeItem = mergeItem;
exports.clear = clear;
exports.getAllKeys = getAllKeys;
exports.multiGet = multiGet;
exports.multiSet = multiSet;
exports.multiRemove = multiRemove;
exports.multiMerge = multiMerge;
/* react-native Not a pure module */
