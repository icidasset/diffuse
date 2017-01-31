module Spinner.View exposing (..)

import Spinner.Styles exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Utils exposing (cssSvgClass)


entry : Svg msg
entry =
    svg
        [ cssSvgClass Spinner
        , height "29px"
        , viewBox "0 0 30 30"
        , width "29px"
        ]
        [ circle
            [ cssSvgClass SpinnerCircle
            , cx "15"
            , cy "15"
            , fill "none"
            , r "14"
            , strokeLinecap "round"
            , strokeWidth "2"
            ]
            []
        ]
