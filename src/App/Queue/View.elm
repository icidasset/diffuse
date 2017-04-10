module Queue.View exposing (..)

import Html exposing (Html, div, h1, li, text, ul)
import Navigation.View as Navigation
import Types exposing (Model, Msg(..))
import Utils exposing (cssClass)


-- Styles

import Styles exposing (Classes(Button, ContentBox))


-- 🍯


entry : Model -> Html Msg
entry model =
    div
        []
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          Navigation.inside
            []

        ------------------------------------
        -- List (TODO)
        ------------------------------------
        , div
            [ cssClass ContentBox ]
            [ h1
                []
                [ text "Queue" ]
            , ul
                []
                (List.map
                    (\item ->
                        li
                            []
                            [ text (Maybe.withDefault "" item.track.tags.artist)
                            , text " - "
                            , text (Maybe.withDefault "" item.track.tags.title)
                            ]
                    )
                    model.queue.future
                )
            ]
        ]
