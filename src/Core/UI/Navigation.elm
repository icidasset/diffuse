module UI.Navigation exposing (Action(..), Icon(..), Label(..), LabelType(..))

import Html.Events.Extra.Mouse as Mouse
import Material.Icons.Types exposing (Coloring)
import Svg exposing (Svg)
import UI.Page exposing (Page)



-- 🌳


type Action msg
    = NavigateToPage Page
    | OpenLinkInNewPage String
    | PerformMsg msg
    | PerformMsgWithMouseEvent (Mouse.Event -> msg)


type Icon msg
    = Icon (Int -> Coloring -> Svg msg)


type Label
    = Label String LabelType


type LabelType
    = Hidden
    | Shown
