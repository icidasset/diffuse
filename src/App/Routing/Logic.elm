module Routing.Logic exposing (locationToMessage, locationToPage)

import Navigation
import Routing.Types exposing (..)
import Sources.Types as Sources
import UrlParser exposing (..)


{-| Parse the location and return a `Msg`.
-}
locationToMessage : Navigation.Location -> Msg
locationToMessage location =
    location
        |> locationToPage
        |> GoToPage


{-| Parse the location and return a `Page`.
-}
locationToPage : Navigation.Location -> Page
locationToPage location =
    location
        |> UrlParser.parsePath route
        |> Maybe.withDefault (ErrorScreen "Page not found.")



-- Routes


route : Parser (Page -> a) a
route =
    oneOf
        [ map (Sources Sources.Index) (s "sources")
        , map (Sources Sources.New) (s "sources" </> s "new")
          -- Other
        , map Index top
        ]
