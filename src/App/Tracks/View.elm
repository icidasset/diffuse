module Tracks.View exposing (entry)

import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Html.Keyed
import Html.Lazy exposing (lazy)
import Json.Decode as Decode
import Material.Icons.Content
import Material.Icons.Toggle
import Navigation.View as Navigation
import Sources.Types exposing (Source)
import Styles exposing (Classes(Button, ContentBox))
import Tracks.Styles exposing (..)
import Tracks.Types exposing (Track)
import Types as TopLevel exposing (Model, Msg)
import Utils exposing (cssClass)
import Variables exposing (colors, colorDerivatives)


-- 🍯


entry : Model -> Html Msg
entry model =
    lazy lazyEntry model.tracks.collection


lazyEntry : List Track -> Html Msg
lazyEntry tracks =
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
            [ tracksTable tracks ]
        ]



-- Views


tracksTable : List Track -> Html Msg
tracksTable tracks =
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
            [ on "click" playTrack ]
            (List.indexedMap tracksTableItem tracks)
        ]


tracksTableItem : Int -> Track -> ( String, Html Msg )
tracksTableItem index track =
    let
        key =
            toString index
    in
        ( key
        , tr
            [ rel key ]
            [ td [] [ starIcon ]
            , td [] [ text track.tags.artist ]
            , td [] [ text track.tags.title ]
            , td [] [ text track.tags.album ]
            ]
        )



-- Events and stuff


playTrack : Decode.Decoder TopLevel.Msg
playTrack =
    Decode.map TopLevel.PlayTrack tableTrackDecoder


tableTrackDecoder : Decode.Decoder String
tableTrackDecoder =
    Decode.oneOf
        [ Decode.at [ "target", "parentNode", "attributes", "rel", "value" ] Decode.string
        , Decode.at [ "target", "attributes", "rel", "value" ] Decode.string
        ]



-- Helpers


starIcon : Html Msg
starIcon =
    Material.Icons.Toggle.star (Color.greyscale 0.0675) 16
