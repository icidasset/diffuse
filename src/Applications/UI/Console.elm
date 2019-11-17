module UI.Console exposing (view)

import Chunky exposing (..)
import Color
import Color.Ext as Color
import Common exposing (Switch(..))
import Conditional exposing (..)
import Css
import Css.Classes as C
import Css.Ext as Css
import Css.Media
import Html exposing (Html, text)
import Html.Attributes exposing (class, style, title)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Material.Icons exposing (Coloring(..))
import Material.Icons.Av as Icons
import Maybe.Extra as Maybe
import Queue
import UI.Css
import UI.Kit
import UI.Queue as Queue
import UI.Reply exposing (Reply(..))



-- 🗺


view : Maybe Queue.Item -> Bool -> Bool -> { stalled : Bool, loading : Bool, playing : Bool } -> ( Float, Float ) -> Html Reply
view activeQueueItem repeat shuffle { stalled, loading, playing } ( position, duration ) =
    chunk
        [ C.antialiased
        , C.mt_1
        , C.text_center
        , C.w_full

        --
        , C.lg__max_w_insulation
        ]
        [ -----------------------------------------
          -- Now Playing
          -----------------------------------------
          chunk
            [ C.text_sm
            , C.italic
            , C.leading_normal
            , C.py_4
            , C.text_white
            ]
            [ if stalled then
                text "Audio connection got interrupted, trying to reconnect ..."

              else if loading then
                text "Loading track ..."

              else
                case Maybe.map .identifiedTrack activeQueueItem of
                    Just ( _, { tags } ) ->
                        Html.span
                            [ onClick ScrollToNowPlaying
                            , class C.cursor_pointer
                            , title "Scroll to track"
                            ]
                            [ text (tags.artist ++ " - " ++ tags.title) ]

                    Nothing ->
                        text "Diffuse"
            ]

        -----------------------------------------
        -- Progress Bar
        -----------------------------------------
        , let
            progress =
                if duration <= 0 then
                    0

                else
                    (position / duration)
                        |> (*) 100
                        |> min 100
                        |> max 0
          in
          brick
            [ on "click" (clickLocationDecoder Seek) ]
            [ C.cursor_pointer
            , C.py_1
            ]
            [ brick
                [ style "background-color" "rgba(255, 255, 255, 0.25)"
                , style "height" "3px"
                ]
                [ C.rounded_sm
                , C.select_none
                ]
                [ brick
                    [ style "background-color" "rgba(255, 255, 255, 0.325)"
                    , style "height" "3px"
                    , style "width" (String.fromFloat progress ++ "%")
                    ]
                    [ "progressBarValue" ]
                    []
                ]
            ]

        -----------------------------------------
        -- Buttons
        -----------------------------------------
        , chunk
            [ C.flex
            , C.justify_between
            , C.mb_3
            , C.mt_4
            , C.select_none
            , C.text_white_90

            --
            , C.sm__justify_center
            ]
            [ button "Toggle repeat" (smallLight repeat) (icon Icons.repeat 18) ToggleRepeat
            , button "Play previous track" lightPlaceHolder (icon Icons.fast_rewind 20) RewindQueue
            , button "" (largeLight playing) play TogglePlayPause
            , button "Play next track" lightPlaceHolder (icon Icons.fast_forward 20) ShiftQueue
            , button "Toggle shuffle" (smallLight shuffle) (icon Icons.shuffle 18) ToggleShuffle
            ]
        ]


button : String -> Html msg -> Html msg -> msg -> Html msg
button t light content msg =
    brick
        [ onClick msg
        , title t
        ]
        [ C.cursor_pointer
        , C.flex
        , C.flex_col
        , C.items_center
        , C.px_1

        --
        , C.sm__mx_8
        ]
        [ brick
            [ style "height" "4px" ]
            []
            [ light ]
        , brick
            [ style "height" "25px" ]
            [ C.flex
            , C.items_center
            , C.my_2
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
        [ C.rounded_full ]
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
        [ C.relative, C.rounded_full ]
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
        [ C.font_bold
        , C.font_display
        , C.relative
        , C.whitespace_no_wrap
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
