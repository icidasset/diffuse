module Element.Types exposing (..)

import Element exposing (Element)
import Styles exposing (Styles)
import Types exposing (Msg)
import Variations exposing (Variations)


-- ⚗️


type alias Node =
    Element Styles Variations Msg


type alias Attr =
    Element.Attribute Variations Msg
