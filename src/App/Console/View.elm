module Console.View exposing (entry)

import Console.Types exposing (Msg(Seek))
import Console.Styles exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (on)
import Json.Decode as Decode
import Material.Icons.Av as Icons
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
            [ text "Ongaku Ryoho" ]

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
                [ div
                    [ cssClass ProgressBarValue, style [ ( "width", "50%" ) ] ]
                    []
                ]
            ]

        -- Buttons
        , div
            [ cssClass ConsoleButtonsContainer ]
            [ a
                [ cssClass ConsoleButton ]
                [ Icons.repeat colorDerivatives.consoleText 18
                , span
                    [ cssClasses
                        [ ConsoleButtonLight
                        ]
                    ]
                    []
                ]
            , a
                [ cssClass ConsoleButton ]
                [ Icons.fast_rewind colorDerivatives.consoleText 20 ]
            , a
                [ cssClass ConsoleButton ]
                [ span
                    []
                    [ text "PLAY" ]
                , span
                    [ cssClasses
                        [ ConsoleButtonLight
                        , ConsoleButtonLightExtended
                        , ConsoleButtonLightExtendedOn
                        ]
                    ]
                    []
                ]
            , a
                [ cssClass ConsoleButton ]
                [ Icons.fast_forward colorDerivatives.consoleText 20 ]
            , a
                [ cssClass ConsoleButton ]
                [ Icons.shuffle colorDerivatives.consoleText 18
                , span
                    [ cssClasses
                        [ ConsoleButtonLight
                        , ConsoleButtonLightOn
                        ]
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
