module Routing.Types exposing (..)

import Playlists.Types as Playlists
import Queue.Types as Queue
import Sources.Types as Sources


-- Messages


type Msg
    = GoToPage Page
    | SetPage Page



-- Model


type alias Model =
    { currentPage : Page }



-- Pages


type Page
    = Abroad
    | Equalizer
    | Index
    | Playlists Playlists.Page
    | Queue Queue.Page
    | Settings
    | Sources Sources.Page
      -- Screens
    | ErrorScreen String
    | MessageScreen String
