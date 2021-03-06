// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var $$Array = require("bs-platform/lib/js/array.js");
var Curry = require("bs-platform/lib/js/curry.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var Js_primitive = require("bs-platform/lib/js/js_primitive.js");
var ReactNative = require("react-native");
var UtilsRN$BsReactNativeWeb = require("../private/utilsRN.js");

function CreateComponent(Impl) {
  var encode_pt_only = function (value) {
    return value[0];
  };
  var imageURISource = function (uri, bundle, method_, headers, body, cache, scale, width, height, _) {
    var tmp = {
      uri: uri,
      width: UtilsRN$BsReactNativeWeb.option_map(encode_pt_only, width),
      height: UtilsRN$BsReactNativeWeb.option_map(encode_pt_only, height)
    };
    if (bundle !== undefined) {
      tmp.bundle = Js_primitive.valFromOption(bundle);
    }
    if (method_ !== undefined) {
      tmp.method = Js_primitive.valFromOption(method_);
    }
    if (headers !== undefined) {
      tmp.headers = Js_primitive.valFromOption(headers);
    }
    if (body !== undefined) {
      tmp.body = Js_primitive.valFromOption(body);
    }
    if (cache !== undefined) {
      tmp.cache = (function () {
            switch (Js_primitive.valFromOption(cache)) {
              case 465819841 : 
                  return "default";
              case 64967513 : 
                  return "reload";
              case -672562089 : 
                  return "force-cache";
              case -74546901 : 
                  return "only-if-cached";
              
            }
          })();
    }
    if (scale !== undefined) {
      tmp.scale = Js_primitive.valFromOption(scale);
    }
    return tmp;
  };
  var defaultURISource = function (uri, scale, width, height, _) {
    var tmp = {
      uri: uri,
      width: UtilsRN$BsReactNativeWeb.option_map(encode_pt_only, width),
      height: UtilsRN$BsReactNativeWeb.option_map(encode_pt_only, height)
    };
    if (scale !== undefined) {
      tmp.scale = Js_primitive.valFromOption(scale);
    }
    return tmp;
  };
  var Event = /* module */[];
  var encodeResizeMode = function (x) {
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
  };
  var encodeSource = function (x) {
    if (x[0] >= 4245324) {
      return x[1];
    } else {
      return $$Array.of_list(x[1]);
    }
  };
  var encodeResizeMethod = function (x) {
    if (x !== -120664438) {
      if (x >= 142301684) {
        return "resize";
      } else {
        return "auto";
      }
    } else {
      return "scale";
    }
  };
  var encodeDefaultSource = function (x) {
    return x[1];
  };
  var make = function (onError, onLayout, onLoad, onLoadEnd, onLoadStart, resizeMode, source, style, testID, resizeMethod, accessibilityLabel, accessible, blurRadius, capInsets, defaultSource, onPartialLoad, onProgress) {
    var partial_arg = {
      onLayout: onLayout,
      onError: onError,
      onLoad: onLoad,
      onLoadEnd: onLoadEnd,
      onLoadStart: onLoadStart,
      resizeMode: UtilsRN$BsReactNativeWeb.option_map(encodeResizeMode, resizeMode),
      source: encodeSource(source),
      style: style,
      testID: testID,
      resizeMethod: UtilsRN$BsReactNativeWeb.option_map(encodeResizeMethod, resizeMethod),
      accessibilityLabel: accessibilityLabel,
      accessible: accessible,
      blurRadius: blurRadius,
      capInsets: capInsets,
      defaultSource: UtilsRN$BsReactNativeWeb.option_map(encodeDefaultSource, defaultSource),
      onPartialLoad: onPartialLoad,
      onProgress: UtilsRN$BsReactNativeWeb.option_map((function (x, y) {
              return Curry._1(x, y.nativeEvent);
            }), onProgress)
    };
    var partial_arg$1 = Impl[/* view */0];
    return (function (param) {
        return ReasonReact.wrapJsForReason(partial_arg$1, partial_arg, param);
      });
  };
  return /* module */[
          (function (prim, prim$1, prim$2, prim$3, prim$4, prim$5, prim$6, prim$7, prim$8, _) {
              var tmp = {
                uri: prim
              };
              if (prim$1 !== undefined) {
                tmp.bundle = Js_primitive.valFromOption(prim$1);
              }
              if (prim$2 !== undefined) {
                tmp.method = Js_primitive.valFromOption(prim$2);
              }
              if (prim$3 !== undefined) {
                tmp.headers = Js_primitive.valFromOption(prim$3);
              }
              if (prim$4 !== undefined) {
                tmp.body = Js_primitive.valFromOption(prim$4);
              }
              if (prim$5 !== undefined) {
                tmp.cache = (function () {
                      switch (Js_primitive.valFromOption(prim$5)) {
                        case 465819841 : 
                            return "default";
                        case 64967513 : 
                            return "reload";
                        case -672562089 : 
                            return "force-cache";
                        case -74546901 : 
                            return "only-if-cached";
                        
                      }
                    })();
              }
              if (prim$6 !== undefined) {
                tmp.scale = Js_primitive.valFromOption(prim$6);
              }
              if (prim$7 !== undefined) {
                tmp.width = Js_primitive.valFromOption(prim$7);
              }
              if (prim$8 !== undefined) {
                tmp.height = Js_primitive.valFromOption(prim$8);
              }
              return tmp;
            }),
          /* imageURISource */imageURISource,
          (function (prim, prim$1, prim$2, prim$3, _) {
              var tmp = {
                uri: prim
              };
              if (prim$1 !== undefined) {
                tmp.scale = Js_primitive.valFromOption(prim$1);
              }
              if (prim$2 !== undefined) {
                tmp.width = Js_primitive.valFromOption(prim$2);
              }
              if (prim$3 !== undefined) {
                tmp.height = Js_primitive.valFromOption(prim$3);
              }
              return tmp;
            }),
          /* defaultURISource */defaultURISource,
          /* Event */Event,
          /* make */make
        ];
}

var Impl = /* module */[/* Image */ReactNative.Image];

function encode_pt_only(value) {
  return value[0];
}

function imageURISource(uri, bundle, method_, headers, body, cache, scale, width, height, _) {
  var tmp = {
    uri: uri,
    width: UtilsRN$BsReactNativeWeb.option_map(encode_pt_only, width),
    height: UtilsRN$BsReactNativeWeb.option_map(encode_pt_only, height)
  };
  if (bundle !== undefined) {
    tmp.bundle = Js_primitive.valFromOption(bundle);
  }
  if (method_ !== undefined) {
    tmp.method = Js_primitive.valFromOption(method_);
  }
  if (headers !== undefined) {
    tmp.headers = Js_primitive.valFromOption(headers);
  }
  if (body !== undefined) {
    tmp.body = Js_primitive.valFromOption(body);
  }
  if (cache !== undefined) {
    tmp.cache = (function () {
          switch (Js_primitive.valFromOption(cache)) {
            case 465819841 : 
                return "default";
            case 64967513 : 
                return "reload";
            case -672562089 : 
                return "force-cache";
            case -74546901 : 
                return "only-if-cached";
            
          }
        })();
  }
  if (scale !== undefined) {
    tmp.scale = Js_primitive.valFromOption(scale);
  }
  return tmp;
}

function defaultURISource(uri, scale, width, height, _) {
  var tmp = {
    uri: uri,
    width: UtilsRN$BsReactNativeWeb.option_map(encode_pt_only, width),
    height: UtilsRN$BsReactNativeWeb.option_map(encode_pt_only, height)
  };
  if (scale !== undefined) {
    tmp.scale = Js_primitive.valFromOption(scale);
  }
  return tmp;
}

var Event = /* module */[];

function encodeResizeMode(x) {
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
}

function encodeSource(x) {
  if (x[0] >= 4245324) {
    return x[1];
  } else {
    return $$Array.of_list(x[1]);
  }
}

function encodeResizeMethod(x) {
  if (x !== -120664438) {
    if (x >= 142301684) {
      return "resize";
    } else {
      return "auto";
    }
  } else {
    return "scale";
  }
}

function encodeDefaultSource(x) {
  return x[1];
}

function make(onError, onLayout, onLoad, onLoadEnd, onLoadStart, resizeMode, source, style, testID, resizeMethod, accessibilityLabel, accessible, blurRadius, capInsets, defaultSource, onPartialLoad, onProgress) {
  var partial_arg = {
    onLayout: onLayout,
    onError: onError,
    onLoad: onLoad,
    onLoadEnd: onLoadEnd,
    onLoadStart: onLoadStart,
    resizeMode: UtilsRN$BsReactNativeWeb.option_map(encodeResizeMode, resizeMode),
    source: encodeSource(source),
    style: style,
    testID: testID,
    resizeMethod: UtilsRN$BsReactNativeWeb.option_map(encodeResizeMethod, resizeMethod),
    accessibilityLabel: accessibilityLabel,
    accessible: accessible,
    blurRadius: blurRadius,
    capInsets: capInsets,
    defaultSource: UtilsRN$BsReactNativeWeb.option_map(encodeDefaultSource, defaultSource),
    onPartialLoad: onPartialLoad,
    onProgress: UtilsRN$BsReactNativeWeb.option_map((function (x, y) {
            return Curry._1(x, y.nativeEvent);
          }), onProgress)
  };
  var partial_arg$1 = Impl[/* view */0];
  return (function (param) {
      return ReasonReact.wrapJsForReason(partial_arg$1, partial_arg, param);
    });
}

function _imageURISource(prim, prim$1, prim$2, prim$3, prim$4, prim$5, prim$6, prim$7, prim$8, _) {
  var tmp = {
    uri: prim
  };
  if (prim$1 !== undefined) {
    tmp.bundle = Js_primitive.valFromOption(prim$1);
  }
  if (prim$2 !== undefined) {
    tmp.method = Js_primitive.valFromOption(prim$2);
  }
  if (prim$3 !== undefined) {
    tmp.headers = Js_primitive.valFromOption(prim$3);
  }
  if (prim$4 !== undefined) {
    tmp.body = Js_primitive.valFromOption(prim$4);
  }
  if (prim$5 !== undefined) {
    tmp.cache = (function () {
          switch (Js_primitive.valFromOption(prim$5)) {
            case 465819841 : 
                return "default";
            case 64967513 : 
                return "reload";
            case -672562089 : 
                return "force-cache";
            case -74546901 : 
                return "only-if-cached";
            
          }
        })();
  }
  if (prim$6 !== undefined) {
    tmp.scale = Js_primitive.valFromOption(prim$6);
  }
  if (prim$7 !== undefined) {
    tmp.width = Js_primitive.valFromOption(prim$7);
  }
  if (prim$8 !== undefined) {
    tmp.height = Js_primitive.valFromOption(prim$8);
  }
  return tmp;
}

function _defaultURISource(prim, prim$1, prim$2, prim$3, _) {
  var tmp = {
    uri: prim
  };
  if (prim$1 !== undefined) {
    tmp.scale = Js_primitive.valFromOption(prim$1);
  }
  if (prim$2 !== undefined) {
    tmp.width = Js_primitive.valFromOption(prim$2);
  }
  if (prim$3 !== undefined) {
    tmp.height = Js_primitive.valFromOption(prim$3);
  }
  return tmp;
}

exports.CreateComponent = CreateComponent;
exports._imageURISource = _imageURISource;
exports.imageURISource = imageURISource;
exports._defaultURISource = _defaultURISource;
exports.defaultURISource = defaultURISource;
exports.Event = Event;
exports.make = make;
/* Impl Not a pure module */
