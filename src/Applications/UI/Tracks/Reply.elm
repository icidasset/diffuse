module UI.Tracks.Reply exposing (Reply(..))

import Coordinates exposing (Coordinates)
import Tracks exposing (..)
import UI.Reply as UI



-- 🌳


type Reply
    = Transcend (List UI.Reply)
      --
    | MarkAsSelected Int { shiftKey : Bool }
    | MoveTrackInSelectedPlaylist { to : Int }
    | ShowTrackMenuWithoutDelay Int { alt : Bool } Coordinates
    | ShowTrackMenuWithSmallDelay Int { alt : Bool } Coordinates
    | SortBy SortBy
    | ToggleFavourite Int
