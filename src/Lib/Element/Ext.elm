module Element.Ext exposing (lazy, lazy2, lazy3, lazySpread, lazySpread2, lazySpread3, lineBreak, onEnterKey)

import Element exposing (Element)
import Element.Attributes
import Element.Events exposing (on)
import Html
import Html.Attributes
import Html.Lazy
import Json.Decode as Json
import Keyboard.Extra exposing (Key, targetKey)
import Styles exposing (Styles(Zed), styles)
import Types as TopLevel
import Variations exposing (Variations)


-- ⚗️


type alias El msg =
    Element Styles Variations msg



-- 🍯


{-| Line break.
-}
lineBreak : Element styles variations msg
lineBreak =
    Element.html (Html.br [] [])


{-| Event binding for the `Enter` key.
-}
onEnterKey : TopLevel.Msg -> Element.Attribute variations TopLevel.Msg
onEnterKey msg =
    on "keydown" (Json.map (ifEnterKey msg) targetKey)


ifEnterKey : TopLevel.Msg -> Key -> TopLevel.Msg
ifEnterKey msg key =
    case key of
        Keyboard.Extra.Enter ->
            msg

        _ ->
            TopLevel.NoOp



-- 💩


{-| Temporary lazy functions for `style-elements`.

TODO: Once `style-elements` has proper support for `lazy` nodes,
remove this hideous hack.

-}
lazy : (a -> El msg) -> a -> El msg
lazy fn a =
    Element.html <| Html.Lazy.lazy (fn >> Element.toHtml styles) a


lazy2 : (a -> b -> El msg) -> a -> b -> El msg
lazy2 fn a b =
    Element.html <| Html.Lazy.lazy2 (\x -> fn x >> Element.toHtml styles) a b


lazy3 : (a -> b -> c -> El msg) -> a -> b -> c -> El msg
lazy3 fn a b c =
    Element.html <| Html.Lazy.lazy3 (\x y -> fn x y >> Element.toHtml styles) a b c


lazySpread : (a -> El msg) -> a -> El msg
lazySpread fn a =
    hideousHack (lazy fn a)


lazySpread2 : (a -> b -> El msg) -> a -> b -> El msg
lazySpread2 fn a b =
    hideousHack (lazy2 fn a b)


lazySpread3 : (a -> b -> c -> El msg) -> a -> b -> c -> El msg
lazySpread3 fn a b c =
    hideousHack (lazy3 fn a b c)


hideousHack : El msg -> El msg
hideousHack =
    Element.section Zed [ Element.Attributes.class "strechin-and-chillin" ]
