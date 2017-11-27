module Console.View exposing (entry)

import Console.Ports
import Console.Types exposing (Msg(..))
import Console.Styles exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick)
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Decode
import Material.Icons.Av as Icons
import Maybe.Extra as Maybe
import Queue.Types exposing (Msg(..))
import Tracks.Types
import Tracks.Utils
import Traits exposing (intoRem)
import Types as TopLevel
import Utils exposing (..)
import Variables exposing (colorDerivatives)


-- 🍯


entry : TopLevel.Model -> Html TopLevel.Msg
entry model =
    div
        [ cssClass Console ]
        [ lazy2 nowPlaying model.queue.activeItem model.console.stalled
        , lazy progress model.queue.activeItem
        , lazy2 buttons model.queue model.console.isPlaying
        ]



-- Now playing


nowPlaying : Maybe Queue.Types.Item -> Bool -> Html TopLevel.Msg
nowPlaying activeItem stalled =
    if stalled then
        div
            [ cssClass NowPlaying
            , onClick (TopLevel.ConsoleMsg Unstall)
            ]
            [ case activeItem of
                Just _ ->
                    text "Your internet connection got interrupted, click to resume."

                Nothing ->
                    text "Isotach"
            ]
    else
        div
            [ cssClass NowPlaying ]
            [ case activeItem of
                Just item ->
                    let
                        track =
                            Tracks.Utils.unindentify item.identifiedTrack
                    in
                        span
                            [ track
                                |> Tracks.Types.ScrollToActiveTrack
                                |> TopLevel.TracksMsg
                                |> onClick
                            ]
                            [ text (track.tags.artist ++ " – " ++ track.tags.title) ]

                Nothing ->
                    text "Isotach"
            ]



-- Progress


progress : Maybe Queue.Types.Item -> Html TopLevel.Msg
progress _ =
    div
        (progressAttributes)
        [ div
            [ cssClass ProgressBarInner ]
            [ div
                [ cssClass ProgressBarValue ]
                []
            ]
        ]


progressAttributes : List (Attribute TopLevel.Msg)
progressAttributes =
    [ cssClass ProgressBar
    , TopLevel.ConsoleMsg
        |> (>>) Seek
        |> decodeClickLocation
        |> on "click"
    ]



-- Buttons


buttons : Queue.Types.Model -> Bool -> Html TopLevel.Msg
buttons queue isPlaying =
    div
        [ cssClass ConsoleButtonsContainer ]
        [ ------------------------------------
          -- Repeat
          ------------------------------------
          a
            [ cssClass ConsoleButton
            , onClick (TopLevel.QueueMsg ToggleRepeat)
            , style [ ( "font-size", intoRem 18 ) ]
            ]
            [ Icons.repeat colorDerivatives.consoleText 18
            , span
                [ queue.repeat
                    |> (\b ->
                            if b == True then
                                [ ConsoleButtonLightOn ]
                            else
                                []
                       )
                    |> List.append [ ConsoleButtonLight ]
                    |> cssClasses
                ]
                []
            ]

        ------------------------------------
        -- Previous
        ------------------------------------
        , a
            [ cssClass ConsoleButton
            , onClick (TopLevel.QueueMsg Rewind)
            , style [ ( "font-size", intoRem 20 ) ]
            ]
            [ Icons.fast_rewind colorDerivatives.consoleText 20 ]

        ------------------------------------
        -- Play / Pause
        ------------------------------------
        , a
            [ cssClass ConsoleButton
            , if isPlaying then
                onClick (TopLevel.ConsoleMsg RequestPause)
              else if Maybe.isNothing queue.activeItem then
                onClick (TopLevel.QueueMsg Shift)
              else
                onClick (TopLevel.ConsoleMsg RequestPlay)
            ]
            [ label
                []
                [ text "PLAY" ]
            , span
                [ isPlaying
                    |> (\b ->
                            if b == True then
                                [ ConsoleButtonLightExtendedOn ]
                            else
                                []
                       )
                    |> List.append [ ConsoleButtonLight, ConsoleButtonLightExtended ]
                    |> cssClasses
                ]
                []
            ]

        ------------------------------------
        -- Next
        ------------------------------------
        , a
            [ cssClass ConsoleButton
            , onClick (TopLevel.QueueMsg Shift)
            , style [ ( "font-size", intoRem 20 ) ]
            ]
            [ Icons.fast_forward colorDerivatives.consoleText 20 ]

        ------------------------------------
        -- Shuffle
        ------------------------------------
        , a
            [ cssClass ConsoleButton
            , onClick (TopLevel.QueueMsg ToggleShuffle)
            , style [ ( "font-size", intoRem 18 ) ]
            ]
            [ Icons.shuffle colorDerivatives.consoleText 18
            , span
                [ queue.shuffle
                    |> (\b ->
                            if b == True then
                                [ ConsoleButtonLightOn ]
                            else
                                []
                       )
                    |> List.append [ ConsoleButtonLight ]
                    |> cssClasses
                ]
                []
            ]
        ]



-- Events and stuff


decodeClickLocation : (Float -> msg) -> Decode.Decoder msg
decodeClickLocation message =
    Decode.map message
        (Decode.map3
            (\a b c -> (a - b) / c)
            (Decode.at [ "pageX" ] Decode.float)
            (Decode.at [ "currentTarget", "offsetLeft" ] Decode.float)
            (Decode.at [ "currentTarget", "clientWidth" ] Decode.float)
        )
