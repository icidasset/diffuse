module UI.Sources exposing (view)

import Chunky exposing (..)
import Html exposing (Html, text)
import Sources
import UI.Core



-- 🗺


view : UI.Core.Model -> Sources.Page -> Html UI.Core.Msg
view model page =
    text "Sources"
