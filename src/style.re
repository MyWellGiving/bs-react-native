type t;

type styleElement = (string, Js.Json.t);

type pt_pct =
  | Pt(float)
  | Pct(float);

let encode_pt_pct = value =>
  switch value {
  | Pt(px) => Encode.float(px)
  | Pct(pct) => Encode.pct(pct)
  };

type pt_pct_auto =
  | Pt(float)
  | Pct(float)
  | Auto;

let encode_pt_pct_auto = value =>
  switch value {
  | Pt(pt) => Encode.float(pt)
  | Pct(pct) => Encode.pct(pct)
  | Auto => Encode.string("auto")
  };

type pt_pct_animated_interpolated =
  | Pt(float)
  | Pct(float)
  | Animated(AnimatedRe.Value.t)
  | Interpolated(AnimatedRe.Interpolation.t);

let encode_pt_pct_animated_interpolated = (value: pt_pct_animated_interpolated) =>
  switch value {
  | Pt(px) => Encode.float(px)
  | Pct(pct) => Encode.pct(pct)
  | Animated(value) => Encode.animatedValue(value)
  | Interpolated(value) => Encode.interpolatedValue(value)
  };

type float_interpolated_animated =
  | Float(float)
  | Animated(AnimatedRe.Value.t)
  | Interpolated(AnimatedRe.Interpolation.t);

let encode_float_interpolated_animated = value =>
  switch value {
  | Float(value) => Encode.float(value)
  | Animated(value) => Encode.animatedValue(value)
  | Interpolated(value) => Encode.interpolatedValue(value)
  };

external flatten : array(t) => t = "%identity";

external to_style : Js.Dict.t(Js.Json.t) => t = "%identity";

external style_to_dict : t => Js.Dict.t(Js.Json.t) = "%identity";

external array_to_style : array(t) => t = "%identity";

let combine = (a, b) => {
  let entries =
    Array.append(
      UtilsRN.dictEntries(style_to_dict(a)),
      UtilsRN.dictEntries(style_to_dict(b))
    );
  UtilsRN.dictFromArray(entries) |> to_style;
};

let concat = styles => array_to_style(Array.of_list(styles));

let floatStyle = (key, value) => (key, Encode.float(value));

let stringStyle = (key, value) => (key, Encode.string(value));

let objectStyle = (key, value) => (key, Encode.object_(value));

let arrayStyle = (key, value) => (key, Encode.array(value));

let style = sarr => sarr |> UtilsRN.dictFromList |> to_style;


/***
 * Layout Props
 */
type alignContent =
  | FlexStart
  | FlexEnd
  | Center
  | Stretch
  | SpaceAround
  | SpaceBetween;

let alignContent = v =>
  stringStyle(
    "alignContent",
    switch v {
    | FlexStart => "flex-start"
    | FlexEnd => "flex-end"
    | Center => "center"
    | Stretch => "stretch"
    | SpaceAround => "space-around"
    | SpaceBetween => "space-between"
    }
  );

type alignItems =
  | FlexStart
  | FlexEnd
  | Center
  | Stretch
  | Baseline;

let alignItems = v =>
  stringStyle(
    "alignItems",
    switch v {
    | FlexStart => "flex-start"
    | FlexEnd => "flex-end"
    | Center => "center"
    | Stretch => "stretch"
    | Baseline => "baseline"
    }
  );

type alignSelf =
  | FlexStart
  | FlexEnd
  | Center
  | Stretch
  | Baseline;

let alignSelf = v =>
  stringStyle(
    "alignSelf",
    switch v {
    | FlexStart => "flex-start"
    | FlexEnd => "flex-end"
    | Center => "center"
    | Stretch => "stretch"
    | Baseline => "baseline"
    }
  );

let aspectRatio = floatStyle("aspectRatio");

let borderBottomWidth = floatStyle("borderBottomWidth");

let borderLeftWidth = floatStyle("borderLeftWidth");

let borderRightWidth = floatStyle("borderRightWidth");

let borderTopWidth = floatStyle("borderTopWidth");

let borderWidth = floatStyle("borderWidth");

type display =
  | Flex
  | None;

let display = v =>
  stringStyle(
    "display",
    switch v {
    | Flex => "flex"
    | None => "none"
    }
  );

let flex = floatStyle("flex");

let flexBasis = value => ("flexBasis", encode_pt_pct(value));

type flexDirection =
  | Row
  | RowReverse
  | Column
  | ColumnReverse;

let flexDirection = v =>
  stringStyle(
    "flexDirection",
    switch v {
    | Row => "row"
    | RowReverse => "row-reverse"
    | Column => "column"
    | ColumnReverse => "column-reverse"
    }
  );

let flexGrow = floatStyle("flexGrow");

let flexShrink = floatStyle("flexShrink");

type flexWrap =
  | Wrap
  | Nowrap;

let flexWrap = v =>
  stringStyle(
    "flexWrap",
    switch v {
    | Wrap => "wrap"
    | Nowrap => "nowrap"
    }
  );

type justifyContent =
  | FlexStart
  | FlexEnd
  | Center
  | Stretch
  | SpaceAround
  | SpaceBetween;

let justifyContent = v =>
  stringStyle(
    "justifyContent",
    switch v {
    | FlexStart => "flex-start"
    | FlexEnd => "flex-end"
    | Center => "center"
    | Stretch => "stretch"
    | SpaceAround => "space-around"
    | SpaceBetween => "space-between"
    }
  );

let margin = value => ("margin", encode_pt_pct_auto(value));

let marginBottom = value => ("marginBottom", encode_pt_pct_auto(value));

let marginHorizontal = value => (
  "marginHorizontal",
  encode_pt_pct_auto(value)
);

let marginLeft = value => ("marginLeft", encode_pt_pct_auto(value));

let marginRight = value => ("marginRight", encode_pt_pct_auto(value));

let marginTop = value => ("marginTop", encode_pt_pct_auto(value));

let marginVertical = value => ("marginVertical", encode_pt_pct_auto(value));

let maxHeight = value => ("maxHeight", encode_pt_pct(value));

let maxWidth = value => ("maxWidth", encode_pt_pct(value));

let minHeight = value => ("minHeight", encode_pt_pct(value));

let minWidth = value => ("minWidth", encode_pt_pct(value));

type overflow =
  | Visible
  | Hidden
  | Scroll;

let overflow = v =>
  stringStyle(
    "overflow",
    switch v {
    | Visible => "visible"
    | Hidden => "hidden"
    | Scroll => "scroll"
    }
  );

let padding = value => ("padding", encode_pt_pct(value));

let paddingBottom = value => ("paddingBottom", encode_pt_pct(value));

let paddingHorizontal = value => ("paddingHorizontal", encode_pt_pct(value));

let paddingLeft = value => ("paddingLeft", encode_pt_pct(value));

let paddingRight = value => ("paddingRight", encode_pt_pct(value));

let paddingTop = value => ("paddingTop", encode_pt_pct(value));

let paddingVertical = value => ("paddingVertical", encode_pt_pct(value));

type position =
  | Absolute
  | Relative;

let position = v =>
  stringStyle(
    "position",
    switch v {
    | Absolute => "absolute"
    | Relative => "relative"
    }
  );

let top = value => ("top", encode_pt_pct_animated_interpolated(value));

let left = value => ("left", encode_pt_pct_animated_interpolated(value));

let right = value => ("right", encode_pt_pct_animated_interpolated(value));

let bottom = value => ("bottom", encode_pt_pct_animated_interpolated(value));

let height = value => ("height", encode_pt_pct_animated_interpolated(value));

let width = value => ("width", encode_pt_pct_animated_interpolated(value));

let zIndex = value => ("zIndex", Encode.int(value));

type direction =
  | Inherit
  | Ltr
  | Rtl;

let direction = v =>
  stringStyle(
    "direction",
    switch v {
    | Inherit => "inherit"
    | Ltr => "ltr"
    | Rtl => "rtl"
    }
  );


/***
 * Shadow Props
 */
let shadowColor = stringStyle("shadowColor");

let shadowOffset = (~height, ~width) =>
  UtilsRN.dictFromArray([|
    ("height", Encode.float(height)),
    ("width", Encode.float(width))
  |])
  |> objectStyle("shadowOffset");

let shadowOpacity = floatStyle("shadowOpacity");

let shadowRadius = floatStyle("shadowRadius");

module Transform = {
  let create_ =
      (
        encoder,
        rotationEncoder,
        perspective,
        rotate,
        rotateX,
        rotateY,
        rotateZ,
        scaleX,
        scaleY,
        translateX,
        translateY,
        skewX,
        skewY
      ) => {
    let opt_values = [|
      ("perspective", [@bs] encoder(perspective)),
      ("rotate", [@bs] rotationEncoder(rotate)),
      ("rotateX", [@bs] rotationEncoder(rotateX)),
      ("rotateY", [@bs] rotationEncoder(rotateY)),
      ("rotateZ", [@bs] rotationEncoder(rotateZ)),
      ("scaleX", [@bs] encoder(scaleX)),
      ("scaleY", [@bs] encoder(scaleY)),
      ("translateX", [@bs] encoder(translateX)),
      ("translateY", [@bs] encoder(translateY)),
      ("skewX", [@bs] encoder(skewX)),
      ("skewY", [@bs] encoder(skewY))
    |];
    let values =
      Array.fold_right(
        (x, acc) =>
          switch x {
          | (key, Some(value)) =>
            let val_ =
              UtilsRN.dictFromArray([|(key, value)|]) |> Encode.object_;
            [val_, ...acc];
          | _ => acc
          },
        opt_values,
        []
      );
    Array.of_list(values) |> arrayStyle("transform");
  };
  let make =
      (
        ~perspective=?,
        ~rotate=?,
        ~rotateX=?,
        ~rotateY=?,
        ~rotateZ=?,
        ~scaleX=?,
        ~scaleY=?,
        ~translateX=?,
        ~translateY=?,
        ~skewX=?,
        ~skewY=?,
        ()
      ) =>
    create_(
      [@bs] (value => UtilsRN.option_map(Encode.float, value)),
      [@bs] (value => UtilsRN.option_map(Encode.string, value)),
      perspective,
      rotate,
      rotateX,
      rotateY,
      rotateZ,
      scaleX,
      scaleY,
      translateX,
      translateY,
      skewX,
      skewY
    );
  let makeAnimated =
      (
        ~perspective=?,
        ~rotate=?,
        ~rotateX=?,
        ~rotateY=?,
        ~rotateZ=?,
        ~scaleX=?,
        ~scaleY=?,
        ~translateX=?,
        ~translateY=?,
        ~skewX=?,
        ~skewY=?,
        ()
      ) =>
    create_(
      [@bs] (value => UtilsRN.option_map(Encode.animatedValue, value)),
      [@bs] (value => UtilsRN.option_map(Encode.animatedValue, value)),
      perspective,
      rotate,
      rotateX,
      rotateY,
      rotateZ,
      scaleX,
      scaleY,
      translateX,
      translateY,
      skewX,
      skewY
    );
  let makeInterpolated =
      (
        ~perspective=?,
        ~rotate=?,
        ~rotateX=?,
        ~rotateY=?,
        ~rotateZ=?,
        ~scaleX=?,
        ~scaleY=?,
        ~translateX=?,
        ~translateY=?,
        ~skewX=?,
        ~skewY=?,
        ()
      ) =>
    create_(
      [@bs] (value => UtilsRN.option_map(Encode.interpolatedValue, value)),
      [@bs] (value => UtilsRN.option_map(Encode.interpolatedValue, value)),
      perspective,
      rotate,
      rotateX,
      rotateY,
      rotateZ,
      scaleX,
      scaleY,
      translateX,
      translateY,
      skewX,
      skewY
    );
};


/***
 * View Props
 */
type backfaceVisibility =
  | Visible
  | Hidden;

let backfaceVisibility = v =>
  stringStyle(
    "backfaceVisibility",
    switch v {
    | Visible => "visible"
    | Hidden => "hidden"
    }
  );

let backgroundColor = stringStyle("backgroundColor");

let borderColor = stringStyle("borderColor");

let borderTopColor = stringStyle("borderTopColor");

let borderRightColor = stringStyle("borderRightColor");

let borderBottomColor = stringStyle("borderBottomColor");

let borderLeftColor = stringStyle("borderLeftColor");

let borderRadius = floatStyle("borderRadius");

let borderTopLeftRadius = floatStyle("borderTopLeftRadius");

let borderTopRightRadius = floatStyle("borderTopRightRadius");

let borderBottomLeftRadius = floatStyle("borderBottomLeftRadius");

let borderBottomRightRadius = floatStyle("borderBottomRightRadius");

type borderStyle =
  | Solid
  | Dotted
  | Dashed;

let string_of_borderStyle =
  fun
  | Solid => "solid"
  | Dotted => "dotted"
  | Dashed => "dashed";

let getPlatformBorderStyle = (style, v) => {
  let propertyName =
    switch (Platform.os()) {
    | exception (Platform.UnknownPlatform(p)) =>
      p === "web" ? style : "borderStyle"
    | IOS(_)
    | Android => "borderStyle"
    };
  stringStyle(propertyName, v |> string_of_borderStyle);
};

let borderStyle = v => stringStyle("borderStyle", v |> string_of_borderStyle);

let borderTopStyle = getPlatformBorderStyle("borderTopStyle");

let borderRightStyle = getPlatformBorderStyle("borderRightStyle");

let borderBottomStyle = getPlatformBorderStyle("borderBottomStyle");

let borderLeftStyle = getPlatformBorderStyle("borderLeftStyle");

let opacity = value => ("opacity", encode_float_interpolated_animated(value));

let elevation = floatStyle("elevation");


/***
 * Text Props
 */
let color = stringStyle("color");

let fontFamily = stringStyle("fontFamily");

let fontSize = value => (
  "fontSize",
  encode_float_interpolated_animated(value)
);

type fontStyle =
  | Normal
  | Italic;

let fontStyle = v =>
  stringStyle(
    "fontStyle",
    switch v {
    | Normal => "normal"
    | Italic => "italic"
    }
  );

let fontWeight = v =>
  stringStyle(
    "fontWeight",
    switch v {
    | `Normal => "normal"
    | `Bold => "bold"
    | `_100 => "100"
    | `_200 => "200"
    | `_300 => "300"
    | `_400 => "400"
    | `_500 => "500"
    | `_600 => "600"
    | `_700 => "700"
    | `_800 => "800"
    | `_900 => "900"
    }
  );

let lineHeight = floatStyle("lineHeight");

type textAlign =
  | Auto
  | Left
  | Right
  | Center
  | Justify;

let textAlign = v =>
  stringStyle(
    "textAlign",
    switch v {
    | Auto => "auto"
    | Left => "left"
    | Right => "right"
    | Center => "center"
    | Justify => "justify"
    }
  );

type textDecorationLine =
  | None
  | Underline
  | LineThrough
  | UnderlineLineThrough;

let textDecorationLine = v =>
  stringStyle(
    "textDecorationLine",
    switch v {
    | None => "none"
    | Underline => "underline"
    | LineThrough => "line-through"
    | UnderlineLineThrough => "underline line-through"
    }
  );

let textShadowColor = stringStyle("textShadowColor");

let textShadowOffset = (~height, ~width) =>
  UtilsRN.dictFromArray([|
    ("height", Encode.float(height)),
    ("width", Encode.float(width))
  |])
  |> objectStyle("textShadowOffset");

let textShadowRadius = floatStyle("textShadowRadius");

let includeFontPadding = value => (
  "includeFontPadding",
  Encode.boolean(Js.Boolean.to_js_boolean(value))
);

type textAlignVertical =
  | Auto
  | Top
  | Bottom
  | Center;

let textAlignVertical = v =>
  stringStyle(
    "textAlignVertical",
    switch v {
    | Auto => "auto"
    | Top => "top"
    | Bottom => "bottom"
    | Center => "center"
    }
  );

let fontVariant = fontVariants =>
  fontVariants
  |> Array.of_list
  |> Array.map(Encode.string)
  |> arrayStyle("fontVariant");

let letterSpacing = floatStyle("letterSpacing");

let textDecorationColor = stringStyle("textDecorationColor");

type textDecorationStyle =
  | Solid
  | Double
  | Dotted
  | Dashed;

let textDecorationStyle = v =>
  stringStyle(
    "textDecorationStyle",
    switch v {
    | Solid => "solid"
    | Double => "double"
    | Dotted => "dotted"
    | Dashed => "dashed"
    }
  );

type writingDirection =
  | Auto
  | Ltr
  | Rtl;

let writingDirection = v =>
  stringStyle(
    "writingDirection",
    switch v {
    | Auto => "auto"
    | Ltr => "ltr"
    | Rtl => "rtl"
    }
  );

type resizeMode =
  | Cover
  | Contain
  | Stretch
  | Repeat
  | Center;


/*** Image props */
let resizeMode = v =>
  stringStyle(
    "resizeMode",
    switch v {
    | Cover => "cover"
    | Contain => "contain"
    | Stretch => "stretch"
    | Repeat => "repeat"
    | Center => "center"
    }
  );

let tintColor = stringStyle("tintColor");

let overlayColor = stringStyle("overlayColor");


/***
 *  Web Props
 */
type listStyleType =
  | Disc
  | Circle
  | Square
  | Decimal
  | Georgian
  | CJKIdeographic
  | Kannada
  | None
  | Inherit
  | Initial
  | Unset
  | Custom(string);

let listStyleType = v =>
  stringStyle(
    "listStyleType",
    switch v {
    | Disc => "disc"
    | Circle => "circle"
    | Square => "square"
    | Decimal => "decimal"
    | Georgian => "georgian"
    | CJKIdeographic => "cjk-ideographic"
    | Kannada => "kannada"
    | None => "none"
    | Inherit => "inherit"
    | Initial => "initial"
    | Unset => "unset"
    | Custom(string) => string
    }
  );

type cursor =
  | Auto
  | Default
  | None
  | ContextMenu
  | Help
  | Pointer
  | Progress
  | Wait
  | Cell
  | Crosshair
  | Text
  | VerticalText
  | Alias
  | Copy
  | Move
  | NoDrop
  | NotAllowed
  | EResize
  | NResize
  | NEResize
  | NWResize
  | SResize
  | SEResize
  | SWResize
  | WResize
  | EWResize
  | NSResize
  | NESWResize
  | NWSEResize
  | ColResize
  | RowResize
  | AllScroll
  | ZoomIn
  | ZoomOut
  | Grab
  | Grabbing;

let cursor = v =>
  stringStyle(
    "cursor",
    switch v {
    | Auto => "auto"
    | Default => "default"
    | None => "none"
    | ContextMenu => "context-menu"
    | Help => "help"
    | Pointer => "pointer"
    | Progress => "progress"
    | Wait => "wait"
    | Cell => "cell"
    | Crosshair => "crosshair"
    | Text => "text"
    | VerticalText => "vertical-text"
    | Alias => "alias"
    | Copy => "copy"
    | Move => "move"
    | NoDrop => "no-drop"
    | NotAllowed => "not-allowed"
    | EResize => "e-resize"
    | NResize => "n-resize"
    | NEResize => "ne-resize"
    | NWResize => "nw-resize"
    | SResize => "s-resize"
    | SEResize => "se-resize"
    | SWResize => "sw-resize"
    | WResize => "w-resize"
    | EWResize => "ew-resize"
    | NSResize => "ns-resize"
    | NESWResize => "nesw-resize"
    | NWSEResize => "nwse-resize"
    | ColResize => "col-resize"
    | RowResize => "row-resize"
    | AllScroll => "all-scroll"
    | ZoomIn => "zoom-in"
    | ZoomOut => "zoom-out"
    | Grab => "grab"
    | Grabbing => "grabbing"
    }
  );

type borderCollapse =
  | Collapse
  | Separate;

let borderCollapse = v =>
  stringStyle(
    "borderCollapse",
    switch v {
    | Collapse => "collapse"
    | Separate => "separate"
    }
  );
