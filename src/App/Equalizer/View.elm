module Equalizer.View exposing (entry)

import Html exposing (..)
import Html.Attributes exposing (..)
import Material.Icons.Navigation as Icons
import Navigation.View as Navigation
import Traits exposing (gr)
import Types as TopLevel
import Utils exposing (..)
import Variables exposing (colorDerivatives)


-- Styles

import Equalizer.Styles exposing (Classes(..))
import Styles exposing (Classes(..))


-- 🍯


entry : TopLevel.Model -> Html TopLevel.Msg
entry model =
    div
        [ cssClass InsulationContent ]
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          Navigation.inside
            [ ( span
                    []
                    [ Icons.arrow_back colorDerivatives.text 16
                    , label [] [ text "Tracks" ]
                    ]
              , "/"
              )
            ]

        ------------------------------------
        -- Content
        ------------------------------------
        , div
            [ cssClass ContentBox ]
            [ h1
                []
                [ text "EQ" ]
            , div
                [ cssClass LogoBackdrop
                , style [ ( "top", backdropOffset ), ( "bottom", "-1px" ) ]
                ]
                []
            , div
                [ cssClass Equalizer ]
                [ div
                    [ cssClass KnobColumn ]
                    [ knob
                    , knobLabel "Volume"
                    ]
                , div
                    [ cssClass KnobColumn ]
                    [ knob
                    , knobLabel "Low"
                    ]
                , div
                    [ cssClass KnobColumn ]
                    [ knob
                    , knobLabel "Mid"
                    ]
                , div
                    [ cssClass KnobColumn ]
                    [ knob
                    , knobLabel "High"
                    ]
                ]
            ]
        ]



-- Knobs


knob : Html msg
knob =
    div
        [ cssClass Knob ]
        [ div [ cssClass KnobLayerA ] []
        , div [ cssClass KnobLayerB ] [ emptyDiv, emptyDiv, emptyDiv, emptyDiv, emptyDiv ]
        , div [ cssClass KnobLayerC ] []
        , div [ cssClass KnobLines ] []
        ]


knobLabel : String -> Html msg
knobLabel lbl =
    div
        [ cssClass KnobLabel ]
        [ text lbl ]



-- Helpers


backdropOffset : String
backdropOffset =
    (gr 7)
        |> .value
        |> (\r -> "calc(" ++ r ++ " + 1px)")


emptyDiv : Html msg
emptyDiv =
    div [] []
