module UI.Javascript.Task.State exposing (..)

import Return
import UI.Javascript.Task.Types exposing (..)


update msg =
    case msg of
        GotCachedTrackBlobUrl a ->
            gotCachedTrackBlobUrl a



-- 🔱


gotCachedTrackBlobUrl _ =
    Return.singleton
