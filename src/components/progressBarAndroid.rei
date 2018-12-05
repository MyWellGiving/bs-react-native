/**
 {b Android-only} component {{:https://facebook.github.io/react-native/docs/progressbarandroid}} used to indicate that the app is loading or there is some activity in the app.
 
 {3 Example of use}
 {4 Horizontal}
 {[
    let component = ReasonReact.statelessComponent("ExampleHorizontal");

    let make = _children => {
      ...component,
      render: _children =>
        <View>
          <ProgressBarAndroid styleAttr=`Horizontal />
          <ProgressBarAndroid styleAttr=`Horizontal color="#2196F3" />
          <ProgressBarAndroid styleAttr=`Horizontal progress=0.5 />
        </View>,
    };
 ]}
 {4 Inverse}
 {[
  let component = ReasonReact.statelessComponent("ExampleInverse");

  let make = _children => {
    ...component,
    render: _children =>
      <View>
        <ProgressBarAndroid styleAttr=`Inverse />
        <ProgressBarAndroid styleAttr=`Inverse color="#2196F3" />
      </View>,
  };
 ]}
 {3 Props}
 {{:\BsReactNative/View-BsReactNative} [View] props}
 {4 color}
 {[
   ~color: string=?,
 ]}
 {4 animating}
 {[
   ~animating: bool=?,
 ]}
 {4 indeterminate}
 If the progress bar will show indeterminate progress. Note that this can only be false if [StyleAttr=`Horizontal]
 {[
   ~indeterminate: bool=?,
 ]}
 {4 progress}
 Value between 0 and 1
 {[
  ~progress: float=?,
 ]}
 {4 styleAttr}
 {[
   ~styleAttr: [
                | `Horizontal
                | `Normal (default)
                | `Small
                | `Large
                | `Inverse
                | `SmallInverse
                | `LargeInverse
            ]
                =?,
 ]}
 {4 testID}
 {[
   ~testID: string=?,
 ]}
*/
let make:
  (
    ~animating: bool=?,
    ~color: string=?,
    ~indeterminate: bool=?,
    ~progress: float=?,
    ~styleAttr: [
                  | `Horizontal
                  | `Inverse
                  | `Large
                  | `LargeInverse
                  | `Normal
                  | `Small
                  | `SmallInverse
                ]
                  =?,
    ~accessibilityLabel: ReasonReact.reactElement=?,
    ~accessible: bool=?,
    ~hitSlop: Types.insets=?,
    ~onAccessibilityTap: unit => unit=?,
    ~onLayout: RNEvent.NativeLayoutEvent.t => unit=?,
    ~onMagicTap: unit => unit=?,
    ~responderHandlers: Types.touchResponderHandlers=?,
    ~pointerEvents: [ | `auto | `none | `boxNone | `boxOnly]=?,
    ~removeClippedSubviews: bool=?,
    ~style: Style.t=?,
    ~testID: string=?,
    ~accessibilityComponentType: [
                                   | `none
                                   | `button
                                   | `radiobutton_checked
                                   | `radiobutton_unchecked
                                 ]
                                   =?,
    ~accessibilityLiveRegion: [ | `none | `polite | `assertive]=?,
    ~collapsable: bool=?,
    ~importantForAccessibility: [ | `auto | `yes | `no | `noHideDescendants]=?,
    ~needsOffscreenAlphaCompositing: bool=?,
    ~renderToHardwareTextureAndroid: bool=?,
    ~accessibilityTraits: list(
                            [
                              | `none
                              | `button
                              | `link
                              | `header
                              | `search
                              | `image
                              | `selected
                              | `plays
                              | `key
                              | `text
                              | `summary
                              | `disabled
                              | `frequentUpdates
                              | `startsMedia
                              | `adjustable
                              | `allowsDirectInteraction
                              | `pageTurn
                            ],
                          )
                            =?,
    ~accessibilityViewIsModal: bool=?,
    ~shouldRasterizeIOS: bool=?,
    array(ReasonReact.reactElement)
  ) =>
  ReasonReact.component(
    ReasonReact.stateless,
    ReasonReact.noRetainedProps,
    ReasonReact.actionless,
  );
