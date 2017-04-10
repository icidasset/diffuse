module Tracks.View exposing (entry)

import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onDoubleClick)
import Maybe.Extra as Maybe
import Material.Icons.Content
import Material.Icons.Toggle
import Navigation.View as Navigation
import Queue.Types as Queue
import Queue.Utils exposing (makeQueueItem)
import Styles exposing (Classes(Button, ContentBox))
import Tracks.Styles exposing (..)
import Tracks.Types exposing (Track)
import Types as TopLevel exposing (Model, Msg(..))
import Utils exposing (cssClass)
import Variables exposing (colors, colorDerivatives)


-- 🍯


entry : Model -> Html Msg
entry model =
    div
        [ cssClass TracksContainer ]
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          Navigation.insideCustom
            [ ( Material.Icons.Content.sort colorDerivatives.text 16, TopLevel.NoOp )
            ]

        ------------------------------------
        -- Table
        ------------------------------------
        , div
            [ cssClass TracksTableContainer ]
            [ tracksTable model ]
        ]



-- Views


tracksTable : Model -> Html Msg
tracksTable model =
    table
        [ cssClass TracksTable ]
        [ thead
            []
            [ th [] []
            , th [] [ text "Artist" ]
            , th [] [ text "Title" ]
            , th [] [ text "Album" ]
            ]
        , tbody
            []
            (List.map
                (\track ->
                    tr
                        [ track
                            |> makeQueueItem True model.timestamp model.sources.collection
                            |> Queue.InjectFirstAndPlay
                            |> QueueMsg
                            |> onDoubleClick
                        ]
                        [ td [] [ Material.Icons.Toggle.star (Color.greyscale 0.0675) 16 ]
                        , td [] [ text (Maybe.withDefault "Unknown" track.tags.artist) ]
                        , td [] [ text (Maybe.withDefault "Unknown" track.tags.title) ]
                        , td [] [ text (Maybe.withDefault "Unknown" track.tags.album) ]
                        ]
                )
                model.queue.tracks
            )
        ]
