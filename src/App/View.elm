module View exposing (entry)

import Html exposing (Html, div)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Types exposing (Model, Msg)


entry : Model -> Html Msg
entry model =
    div
        []
        [ div
            [ class "centered" ]
            [ spinner ]
        , backgroundImage model.backgroundImage
        ]



-- Bits


backgroundImage : String -> Svg msg
backgroundImage imageUrl =
    svg
        [ class "background-image" ]
        [ defs
            []
            [ Svg.filter
                [ id "background-image__blur-filter" ]
                [ feGaussianBlur
                    [ stdDeviation "10"
                    ]
                    []
                ]
            ]
        , image
            [ x "-2.5%"
            , y "-2.5%"
            , width "105%"
            , height "105%"
            , preserveAspectRatio "xMinYMin slice"
            , xlinkHref imageUrl
            , Svg.Attributes.filter "url(#background-image__blur-filter)"
            ]
            []
        ]


spinner : Svg msg
spinner =
    svg
        [ class "spinner"
        , height "29px"
        , viewBox "0 0 30 30"
        , width "29px"
        ]
        [ circle
            [ class "spinner__path"
            , cx "15"
            , cy "15"
            , fill "none"
            , r "14"
            , strokeLinecap "round"
            , strokeWidth "2"
            ]
            []
        ]
