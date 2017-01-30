module Routing.Types exposing (..)


type Msg
    = SetPage Page


type alias Model =
    { currentPage : Page }


type Page
    = Index
    | ErrorScreen String
