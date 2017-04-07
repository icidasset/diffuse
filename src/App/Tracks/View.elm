module Tracks.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe.Extra as Maybe
import Navigation.View as Navigation
import Styles exposing (Classes(Button, ContentBox))
import Types exposing (Model, Msg(..))
import Utils exposing (cssClass)


-- 🍯


entry : Model -> Html Msg
entry model =
    ul
        []
        (List.map
            (\track ->
                li
                    []
                    ([ track.tags.artist
                     , track.tags.title
                     ]
                        |> List.filter (Maybe.isJust)
                        |> List.map (Maybe.withDefault "")
                        |> String.join " – "
                        |> text
                        |> List.singleton
                    )
            )
            model.tracks.collection
        )
