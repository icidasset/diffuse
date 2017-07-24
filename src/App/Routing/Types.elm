module Routing.Types exposing (..)

import Queue.Types as Queue
import Sources.Types as Sources


type Msg
    = GoToPage Page
    | GoToUrl String


type alias Model =
    { currentPage : Page }


type Page
    = About
    | Equalizer
    | ErrorScreen String
    | Index
    | Queue Queue.Page
    | Settings
    | Sources Sources.Page
