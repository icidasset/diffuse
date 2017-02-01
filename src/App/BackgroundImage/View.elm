module BackgroundImage.View exposing (..)

import BackgroundImage.Styles exposing (..)
import Css.Helpers exposing (identifierToString)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Utils exposing (cssSvgClass)


entry : String -> Svg msg
entry imageUrl =
    let
        imageFilterId =
            identifierToString "" BackgroundImageFilter

        filterUrl =
            String.concat [ "url(#", imageFilterId, ")" ]
    in
        svg
            [ cssSvgClass BackgroundImage ]
            [ defs
                []
                [ Svg.filter
                    [ id imageFilterId ]
                    [ feGaussianBlur [ stdDeviation "10" ] [] ]
                ]
            , image
                [ x "-2.5%"
                , y "-2.5%"
                , width "105%"
                , height "105%"
                , preserveAspectRatio "xMinYMin slice"
                , xlinkHref imageUrl
                , Svg.Attributes.filter filterUrl
                ]
                []
            ]
