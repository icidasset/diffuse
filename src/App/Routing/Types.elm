module Routing.Types exposing (..)

import Sources.Types as Sources


type Msg
    = GoToPage Page
    | GoToUrl String


type alias Model =
    { currentPage : Page }


type Page
    = ErrorScreen String
    | Index
    | Sources Sources.Page
