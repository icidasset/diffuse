module Console.View exposing (entry)

import Console.Ports
import Console.Types exposing (Msg(..))
import Console.Styles exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Material.Icons.Av as Icons
import Maybe.Extra as Maybe
import Queue.Types exposing (Msg(Rewind, Shift, ToggleRepeat, ToggleShuffle))
import Types as TopLevel
import Utils exposing (..)
import Variables exposing (colorDerivatives)


-- 🍯


entry : TopLevel.Model -> Html TopLevel.Msg
entry model =
    div
        [ cssClass Console ]
        [ div
            [ cssClass NowPlaying ]
            [ case model.queue.activeItem of
                Just item ->
                    ([ item.track.tags.artist
                     , item.track.tags.title
                     ]
                        |> List.filter (Maybe.isJust)
                        |> List.map (Maybe.withDefault "")
                        |> String.join " – "
                        |> text
                    )

                Nothing ->
                    text "Ongaku Ryoho"
            ]

        -- Progress
        , div
            [ cssClass ProgressBar
            , TopLevel.ConsoleMsg
                |> (>>) Seek
                |> decodeClickLocation
                |> on "click"
            ]
            [ div
                [ cssClass ProgressBarInner ]
                [ let
                    width =
                        model.console.progress
                            |> (*) 100
                            |> toString
                  in
                    div
                        [ cssClass ProgressBarValue
                        , style [ ( "width", width ++ "%" ) ]
                        ]
                        []
                ]
            ]

        -- Buttons
        , div
            [ cssClass ConsoleButtonsContainer ]
            [ a
                [ cssClass ConsoleButton
                , onClick (TopLevel.QueueMsg ToggleRepeat)
                ]
                [ Icons.repeat colorDerivatives.consoleText 18
                , span
                    [ model.queue.repeat
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
            , a
                [ cssClass ConsoleButton
                , onClick (TopLevel.QueueMsg Rewind)
                ]
                [ Icons.fast_rewind colorDerivatives.consoleText 20 ]
            , a
                [ cssClass ConsoleButton
                , if model.console.isPlaying then
                    onClick (TopLevel.ConsoleMsg RequestPause)
                  else if Maybe.isNothing model.queue.activeItem then
                    onClick (TopLevel.QueueMsg Shift)
                  else
                    onClick (TopLevel.ConsoleMsg RequestPlay)
                ]
                [ label
                    []
                    [ text "PLAY" ]
                , span
                    [ model.console.isPlaying
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
            , a
                [ cssClass ConsoleButton
                , onClick (TopLevel.QueueMsg Shift)
                ]
                [ Icons.fast_forward colorDerivatives.consoleText 20 ]
            , a
                [ cssClass ConsoleButton
                , onClick (TopLevel.QueueMsg ToggleShuffle)
                ]
                [ Icons.shuffle colorDerivatives.consoleText 18
                , span
                    [ model.queue.shuffle
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
