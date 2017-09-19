module Routing.Types exposing (..)

import Queue.Types as Queue
import Sources.Types as Sources


type Msg
    = GoToPage Page
    | SetPage Page


type alias Model =
    { currentPage : Page }


type Page
    = Abroad
    | Equalizer
    | ErrorScreen String
    | Index
    | Queue Queue.Page
    | Settings
    | Sources Sources.Page
