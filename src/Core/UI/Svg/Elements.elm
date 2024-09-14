module UI.Svg.Elements exposing (dropboxLogo, ipfsLogo, loading, loadingWithSize, remoteStorageLogo)

import Svg exposing (..)
import Svg.Attributes exposing (..)



-- LOGOS


ipfsLogo : Int -> Svg Never
ipfsLogo size =
    svg
        [ height (String.fromInt size)
        , viewBox "0 0 511.99999 511.99998"
        , width (String.fromInt size)
        ]
        [ -- Group 1
          ----------
          g
            [ transform "translate(-50.017 -515.51)" ]
            [ Svg.path
                [ d "m283.13 546.35-160.74 92.806c0.32126 2.8543 0.32125 5.7352 0 8.5894l160.75 92.806c13.554-10.001 32.043-10.001 45.597 0l160.75-92.807c-0.32126-2.8543-0.32293-5.7338-0.001-8.588l-160.74-92.806c-13.554 10.001-32.044 10.001-45.599 0zm221.79 127.03-160.92 93.84c1.884 16.739-7.3611 32.751-22.799 39.489l0.18062 184.58c2.6325 1.1489 5.1267 2.5886 7.438 4.294l160.75-92.805c-1.884-16.739 7.3611-32.752 22.799-39.49v-185.61c-2.6325-1.1489-5.1281-2.5886-7.4394-4.294zm-397.81 1.0315c-2.3112 1.7054-4.8054 3.1465-7.438 4.2954v185.61c15.438 6.7378 24.683 22.75 22.799 39.489l160.74 92.806c2.3112-1.7054 4.8069-3.1465 7.4394-4.2954v-185.61c-15.438-6.7378-24.683-22.75-22.799-39.489l-160.74-92.81z"
                , fill "currentColor"
                ]
                []
            ]

        -- Group 2
        ----------
        , g
            [ fill "currentColor"
            , transform "translate(0 -196.66)"
            ]
            [ Svg.path
                [ d "m256 708.66 221.7-128v-256l-221.7 128v256z"
                , fillOpacity "1"
                ]
                []
            , Svg.path
                [ d "m256 708.66v-256l-221.7-128v256l221.7 128z"
                , fillOpacity ".75"
                ]
                []
            , Svg.path
                [ d "m34.298 324.66 221.7 128 221.7-128-221.7-128-221.7 128z"
                , fillOpacity ".5"
                ]
                []
            ]
        ]


dropboxLogo : Int -> Svg Never
dropboxLogo size =
    svg
        [ height (String.fromInt size)
        , viewBox "0 0 43 40"
        , width (String.fromInt size)
        ]
        [ Svg.path
            [ d "m12.5 0l-12.5 8.1 8.7 7 12.5-7.8-8.7-7.3zm-12.5 21.9l12.5 8.2 8.7-7.3-12.5-7.7-8.7 6.8zm21.2 0.9l8.8 7.3 12.4-8.1-8.6-6.9-12.6 7.7zm21.2-14.7l-12.4-8.1-8.8 7.3 12.6 7.8 8.6-7zm-21.1 16.3l-8.8 7.3-3.7-2.5v2.8l12.5 7.5 12.5-7.5v-2.8l-3.8 2.5-8.7-7.3z"

            --
            , fill "currentColor"
            ]
            []
        ]


remoteStorageLogo : Int -> Svg Never
remoteStorageLogo size =
    svg
        [ clipRule "evenodd"
        , fillRule "evenodd"
        , height (String.fromInt size)
        , imageRendering "optimizeQuality"
        , shapeRendering "geometricPrecision"
        , textRendering "geometricPrecision"
        , viewBox "0 0 739 853"
        , width (String.fromInt size)
        ]
        [ polygon
            [ points "370,754 0,542 0,640 185,747 370,853 554,747 739,640 739,525 739,525 739,476 739,427 739,378 653,427 370,589 86,427 86,427 86,361 185,418 370,524 554,418 653,361 739,311 739,213 739,213 554,107 370,0 185,107 58,180 144,230 228,181 370,100 511,181 652,263 370,425 87,263 87,263 0,213 0,213 0,311 0,378 0,427 0,476 86,525 185,582 370,689 554,582 653,525 653,590 653,592"
            , fill "currentColor"
            ]
            []
        ]



-- LOADING ANIMATION


loading : Svg Never
loading =
    loadingWithSize 29


loadingWithSize : Int -> Svg Never
loadingWithSize size =
    svg
        [ class "loading-animation"
        , height (String.fromInt size)
        , viewBox "0 0 30 30"
        , width (String.fromInt size)
        ]
        [ circle
            [ class "loading-animation__circle"
            , cx "15"
            , cy "15"
            , fill "none"
            , r "14"
            , strokeLinecap "round"
            , strokeWidth "2"
            ]
            []
        ]
