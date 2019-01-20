module UI.Reply exposing (Reply(..))

import Alien
import Json.Decode as Json
import Sources exposing (Source)
import UI.Page exposing (Page)



-- 🌳


type Reply
    = AddSourceToCollection Source
    | Chill
    | GoToPage Page
    | ProcessSources
    | SaveEnclosedUserData
    | SaveFavourites
    | SaveSources
    | SaveTracks
