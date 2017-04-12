module Tracks.View exposing (entry)

import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Html.Keyed
import Html.Lazy exposing (lazy)
import Json.Decode as Decode
import Material.Icons.Content
import Material.Icons.Toggle
import Navigation.View as Navigation
import Sources.Types exposing (Source)
import Styles exposing (Classes(Button, ContentBox))
import Tracks.Styles exposing (..)
import Tracks.Types exposing (Msg(..), SortBy(..), Track)
import Types as TopLevel exposing (Model, Msg)
import Utils exposing (cssClass)
import Variables exposing (colors, colorDerivatives)


-- 🍯


entry : Model -> Html TopLevel.Msg
entry model =
    lazy lazyEntry model.tracks.collection


lazyEntry : List Track -> Html TopLevel.Msg
lazyEntry tracks =
    div
        [ cssClass TracksContainer ]
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          div
            [ cssClass TracksNavigation ]
            [ Navigation.insideCustom
                [ ( Material.Icons.Content.sort colorDerivatives.text 16, TopLevel.NoOp )
                ]
            ]

        ------------------------------------
        -- Table
        ------------------------------------
        , div
            [ cssClass TracksTableContainer ]
            [ tracksTable tracks ]
        ]



-- Views


tracksTable : List Track -> Html TopLevel.Msg
tracksTable tracks =
    table
        [ cssClass TracksTable ]
        [ thead
            []
            [ th [ style [ ( "width", "4.50%" ) ] ] []
            , th [ style [ ( "width", "37.5%" ) ], onClick (sortBy Title) ] [ text "Title" ]
            , th [ style [ ( "width", "29.0%" ) ], onClick (sortBy Artist) ] [ text "Artist" ]
            , th [ style [ ( "width", "29.0%" ) ], onClick (sortBy Album) ] [ text "Album" ]
            ]
        , Html.Keyed.node
            "tbody"
            [ on "dblclick" playTrack ]
            (List.indexedMap tracksTableItem tracks)
        ]


tracksTableItem : Int -> Track -> ( String, Html TopLevel.Msg )
tracksTableItem index track =
    let
        key =
            toString index
    in
        ( key
        , tr
            [ rel key ]
            [ td [] [{- starIcon -}]
            , td [] [ text track.tags.title ]
            , td [] [ text track.tags.artist ]
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


sortBy : SortBy -> TopLevel.Msg
sortBy =
    TopLevel.TracksMsg << SortBy



-- Helpers


starIcon : Html TopLevel.Msg
starIcon =
    Material.Icons.Toggle.star (Color.greyscale 0.0675) 16
