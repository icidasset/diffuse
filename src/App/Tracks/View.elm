module Tracks.View exposing (entry)

import Color
import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onDoubleClick)
import Html.Keyed
import Html.Lazy exposing (lazy3)
import Maybe.Extra as Maybe
import Material.Icons.Content
import Material.Icons.Toggle
import Navigation.View as Navigation
import Queue.Types as Queue
import Queue.Utils exposing (makeQueueItem)
import Sources.Types exposing (Source)
import Styles exposing (Classes(Button, ContentBox))
import Tracks.Styles exposing (..)
import Tracks.Types exposing (Track)
import Types as TopLevel exposing (Model, Msg(..))
import Utils exposing (cssClass)
import Variables exposing (colors, colorDerivatives)


-- 🍯


entry : Model -> Html Msg
entry model =
    lazy3
        lazyEntry
        model.timestamp
        model.sources.collection
        model.queue.tracks


lazyEntry : Date -> List Source -> List Track -> Html Msg
lazyEntry timestamp sources tracks =
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
            [ tracksTable timestamp sources tracks ]
        ]



-- Views


tracksTable : Date -> List Source -> List Track -> Html Msg
tracksTable timestamp sources tracks =
    table
        [ cssClass TracksTable ]
        [ thead
            []
            [ th [] []
            , th [] [ text "Artist" ]
            , th [] [ text "Title" ]
            , th [] [ text "Album" ]
            ]
        , Html.Keyed.node
            "tbody"
            []
            (List.map
                (\track ->
                    ( track.sourceId ++ track.path
                    , tr
                        [ track
                            |> makeQueueItem True timestamp sources
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
                )
                tracks
            )
        ]
