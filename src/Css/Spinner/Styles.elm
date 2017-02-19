module Spinner.Styles exposing (..)

import Css exposing (..)
import Spinner.Variables as Variables
import String.Interpolate exposing (interpolate)
import Variables exposing (..)


type Classes
    = Spinner
    | SpinnerCircle



-- 🦄


styles : List Snippet
styles =
    [ class Spinner
        [ Variables.animationDuration
            |> toString
            |> List.singleton
            |> interpolate "spinnerRotator {0}s linear infinite"
            |> property "animation"
        ]
    , class SpinnerCircle
        [ property "animation" circleAnimation
        , property "stroke-dasharray" (toString Variables.offset)
        , property "stroke-dashoffset" "0"
        , property "transform-origin" "center"
        ]
    ]


circleAnimation : String
circleAnimation =
    """
    spinnerDash {0}s ease-in-out infinite,
    spinnerColors {1}s ease-in-out infinite
    """
        |> String.trim
        |> (flip interpolate)
            [ toString <| Variables.animationDuration
            , toString <| Variables.animationDuration * 4
            ]



-- Keyframes


keyframes : String
keyframes =
    interpolate
        """
        @keyframes spinnerRotator {
            0% { transform: rotate(0deg); }
            100% { transform: rotate(270deg); }
        }

        @keyframes spinnerColors {
          0% { stroke: {0}; }
          25% { stroke: {1}; }
          50% { stroke: {2}; }
          75% { stroke: {3}; }
          100% { stroke: {0}; }
        }

        @keyframes spinnerDash {
          0% {
            stroke-dashoffset: {4};
          }
          50% {
            stroke-dashoffset: {5};
            transform: rotate(135deg);
          }
          100% {
            stroke-dashoffset: {4};
            transform: rotate(450deg);
          }
        }
        """
        [ colors.base08
        , colors.base0A
        , colors.base0B
        , colors.base0D
        , toString <| Variables.offset
        , toString <| Variables.offset / 4
        ]
