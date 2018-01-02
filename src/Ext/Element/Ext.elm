module Element.Ext exposing (lazy, lazy2, lazy3, lineBreak, onEnterKey)

import Element exposing (Element)
import Element.Events exposing (on)
import Html
import Html.Lazy
import Keyboard.Extra exposing (Key, targetKey)
import Json.Decode as Json
import Styles exposing (Styles, styles)
import Types as TopLevel
import Variations exposing (Variations)


-- ⚗️


type alias El msg =
    Element Styles Variations msg



-- 🍯


{-| Temporary lazy functions for `style-elements`.
-}
lazy : (a -> El msg) -> a -> El msg
lazy fn a =
    Element.html <| Html.Lazy.lazy (\x -> x |> fn |> Element.toHtml styles) a


lazy2 : (a -> b -> El msg) -> a -> b -> El msg
lazy2 fn a b =
    Element.html <| Html.Lazy.lazy2 (\x y -> y |> fn x |> Element.toHtml styles) a b


lazy3 : (a -> b -> c -> El msg) -> a -> b -> c -> El msg
lazy3 fn a b c =
    Element.html <| Html.Lazy.lazy3 (\x y z -> z |> fn x y |> Element.toHtml styles) a b c


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
