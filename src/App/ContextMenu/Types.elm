module ContextMenu.Types exposing (..)

import Mouse
import Svg exposing (Svg)


-- Messages


type Msg
    = Hide
    | ShowPlaylistMenu String Mouse.Position
    | ShowSourceMenu String Mouse.Position



-- Model


type alias Model msg =
    { instance : Maybe (ContextMenu msg)
    }



-- Context Menu


type ContextMenu msg
    = ContextMenu (ContextMenuItems msg) Mouse.Position


type alias ContextMenuItems msg =
    List ( Svg msg, String, msg )
