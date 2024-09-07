module Themes.Sunrise.Console exposing (view)

import Chunky exposing (..)
import Conditional exposing (..)
import Html exposing (Html, text)
import Html.Attributes exposing (style, title)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Material.Icons.Round as Icons
import Material.Icons.Types exposing (Coloring(..))
import Maybe.Extra as Maybe
import UI.Audio.Types exposing (AudioLoadingState(..), NowPlaying, nowPlayingIdentifiedTrack)
import UI.Queue.Types as Queue
import UI.Tracks.Types as Tracks
import UI.Types exposing (Msg(..))



-- 🗺


view :
    Maybe NowPlaying
    -> Bool
    -> Bool
    -> Html Msg
view nowPlaying repeat shuffle =
    chunk
        [ "antialiased"
        , "mt-1"
        , "text-center"
        , "w-full"

        --
        , "lg:max-w-insulation"
        ]
        [ -----------------------------------------
          -- Now Playing
          -----------------------------------------
          chunk
            [ "text-sm"
            , "italic"
            , "leading-normal"
            , "py-4"
            , "text-white"
            ]
            [ case Maybe.map .loadingState nowPlaying of
                Nothing ->
                    text "Diffuse"

                Just Loading ->
                    text "Loading track ..."

                Just Loaded ->
                    case Maybe.map nowPlayingIdentifiedTrack nowPlaying of
                        Just ( _, { tags } ) ->
                            slab
                                Html.span
                                [ onClick (TracksMsg Tracks.ScrollToNowPlaying)
                                , title "Scroll to track"
                                ]
                                [ "cursor-pointer" ]
                                [ case tags.artist of
                                    Just artist ->
                                        text (artist ++ " - " ++ tags.title)

                                    Nothing ->
                                        text tags.title
                                ]

                        Nothing ->
                            text "Diffuse"

                -----------------------------------------
                -- Errors
                -----------------------------------------
                Just DecodeError ->
                    text "(!) An error occurred while decoding the audio"

                Just NetworkError ->
                    text "Waiting until your internet connection comes back online ..."

                Just NotSupportedError ->
                    text "(!) Your browser does not support playing this type of audio"

            -- Just NotSupportedOrMissing ->
            --     text "The audio is missing or is in a format not supported by your browser."
            ]

        -----------------------------------------
        -- Progress Bar
        -----------------------------------------
        , let
            maybeDuration =
                Maybe.andThen .duration nowPlaying

            maybePosition =
                Maybe.map .playbackPosition nowPlaying

            progress =
                case ( maybeDuration, maybePosition ) of
                    ( Just duration, Just position ) ->
                        if duration <= 0 then
                            0

                        else
                            (position / duration)
                                |> (*) 100
                                |> min 100
                                |> max 0

                    _ ->
                        0
          in
          brick
            (case nowPlaying of
                Just { item } ->
                    item.identifiedTrack
                        |> Tuple.second
                        |> .id
                        |> (\id ->
                                \float -> Seek { progress = float, trackId = id }
                           )
                        |> clickLocationDecoder
                        |> on "click"
                        |> List.singleton

                Nothing ->
                    []
            )
            [ "cursor-pointer"
            , "py-1"
            ]
            [ brick
                [ style "background-color" "rgba(255, 255, 255, 0.25)"
                , style "height" "3px"
                ]
                [ "rounded-sm"
                , "select-none"
                ]
                [ brick
                    [ style "background-color" "rgba(255, 255, 255, 0.325)"
                    , style "height" "3px"
                    , style "width" (String.fromFloat progress ++ "%")
                    ]
                    [ "progressBarValue"
                    , "rounded-sm"
                    ]
                    []
                ]
            ]

        -----------------------------------------
        -- Buttons
        -----------------------------------------
        , chunk
            [ "flex"
            , "justify-between"
            , "mb-3"
            , "mt-4"
            , "select-none"
            , "text-white-90"

            --
            , "sm:justify-center"
            ]
            [ button "Toggle repeat"
                (smallLight repeat)
                (icon Icons.repeat 18)
                (QueueMsg Queue.ToggleRepeat)

            --
            , button
                "Play previous track"
                lightPlaceHolder
                (icon Icons.fast_rewind 20)
                (QueueMsg Queue.Rewind)

            --
            , let
                isPlaying =
                    Maybe.unwrap False .isPlaying nowPlaying
              in
              button
                ""
                (largeLight isPlaying)
                play
                TogglePlay

            --
            , button
                "Play next track"
                lightPlaceHolder
                (icon Icons.fast_forward 20)
                (QueueMsg Queue.Shift)

            --
            , button
                "Toggle shuffle"
                (smallLight shuffle)
                (icon Icons.shuffle 18)
                (QueueMsg Queue.ToggleShuffle)
            ]
        ]


button : String -> Html msg -> Html msg -> msg -> Html msg
button t light content msg =
    brick
        [ onClick msg
        , title t
        ]
        [ "cursor-pointer"
        , "flex"
        , "flex-col"
        , "items-center"
        , "px-1"

        --
        , "sm:mx-8"
        ]
        [ brick
            [ style "height" "4px" ]
            []
            [ light ]
        , brick
            [ style "height" "25px" ]
            [ "flex"
            , "items-center"
            , "my-2"
            ]
            [ content ]
        ]


smallLight : Bool -> Html msg
smallLight isOn =
    brick
        [ style "height" "4px"
        , style "width" "4px"

        --
        , style "background-color" <|
            ifThenElse
                isOn
                "rgb(157, 174, 255)"
                "rgba(255, 255, 255, 0.25)"
        ]
        [ "rounded-full" ]
        []


largeLight : Bool -> Html msg
largeLight isOn =
    brick
        [ style "height" "4px"
        , style "left" "-2px"
        , style "width" "17px"

        --
        , style "background-color" <|
            ifThenElse
                isOn
                "rgb(198, 254, 153)"
                "rgba(255, 255, 255, 0.25)"
        ]
        [ "relative", "rounded-full" ]
        []


lightPlaceHolder : Html msg
lightPlaceHolder =
    Html.div
        [ style "height" "4px" ]
        []


play : Html msg
play =
    brick
        [ style "font-size" "11.25px"
        , style "letter-spacing" "3.75px"
        ]
        [ "font-bold"
        , "font-display"
        , "relative"
        , "whitespace-nowrap"
        ]
        [ text "PLAY" ]



-- ⚗️


icon : (Int -> Coloring -> Html msg) -> Int -> Html msg
icon iconFunction int =
    iconFunction int Inherit



-- EVENTS


clickLocationDecoder : (Float -> msg) -> Decode.Decoder msg
clickLocationDecoder message =
    Decode.map message
        (Decode.map2
            (\a b -> a / b)
            (Decode.at [ "offsetX" ] Decode.float)
            (Decode.at [ "currentTarget", "clientWidth" ] Decode.float)
        )
