module UI.Backdrop exposing (..)

import Chunky exposing (..)
import Color
import Html exposing (Html)
import Html.Attributes exposing (src, style)
import Html.Events exposing (on)
import Html.Lazy as Lazy
import Json.Decode
import Return exposing (return)
import UI.Ports as Ports
import UI.Types exposing (..)
import UI.User.State.Export as User



-- ⛩


default : String
default =
    "21.jpg"


options : List ( String, String )
options =
    [ ( "1.jpg", "Option 1" )
    , ( "2.jpg", "Option 2" )
    , ( "3.jpg", "Option 3" )
    , ( "4.jpg", "Option 4" )
    , ( "5.jpg", "Option 5" )
    , ( "6.jpg", "Option 6" )
    , ( "7.jpg", "Option 7" )
    , ( "8.jpg", "Option 8" )
    , ( "9.jpg", "Option 9" )
    , ( "10.jpg", "Option 10" )
    , ( "11.jpg", "Option 11" )
    , ( "12.jpg", "Option 12" )
    , ( "13.jpg", "Option 13" )
    , ( "14.jpg", "Option 14" )
    , ( "15.jpg", "Option 15" )
    , ( "16.jpg", "Option 16" )
    , ( "17.jpg", "Option 17" )
    , ( "18.jpg", "Option 18" )
    , ( "19.jpg", "Option 19" )
    , ( "20.jpg", "Option 20" )
    , ( "21.jpg", "Option 21 (default)" )
    , ( "22.jpg", "Option 22" )
    , ( "23.jpg", "Option 23" )
    , ( "24.jpg", "Option 24" )
    , ( "25.jpg", "Option 25" )
    , ( "26.jpg", "Option 26" )
    , ( "27.jpg", "Option 27" )
    , ( "28.jpg", "Option 28" )
    , ( "29.jpg", "Option 29" )
    , ( "30.jpg", "Option 30" )
    ]



-- 📣


extractedBackdropColor : { r : Int, g : Int, b : Int } -> Manager
extractedBackdropColor { r, g, b } model =
    Return.singleton { model | extractedBackdropColor = Just (Color.rgb255 r g b) }


chooseBackdrop : String -> Manager
chooseBackdrop backdrop model =
    User.saveSettings { model | chosenBackdrop = Just backdrop }


loadBackdrop : String -> Manager
loadBackdrop backdrop model =
    return
        { model | loadedBackdrops = model.loadedBackdrops ++ [ backdrop ] }
        (Ports.pickAverageBackgroundColor backdrop)


setDefault : Manager
setDefault model =
    Return.singleton { model | chosenBackdrop = Just default }



-- 🗺


view : Model -> Html Msg
view model =
    chunk
        [ "fixed"
        , "-inset-px"
        , "z-0"
        ]
        [ Lazy.lazy chosen model.chosenBackdrop
        , Lazy.lazy2 loaded model.loadedBackdrops model.fadeInBackdrop

        -- Shadow
        ---------
        , brick
            [ style "background" "linear-gradient(#0000, rgba(0, 0, 0, 0.175))" ]
            [ "absolute"
            , "bottom-0"
            , "h-64"
            , "inset-x-0"
            , "z-10"
            ]
            []
        ]


backgroundImage : String -> Html.Attribute msg
backgroundImage backdrop =
    style "background-image" ("url(images/Background/" ++ backdrop ++ ")")


backgroundPositioning : String -> Html.Attribute msg
backgroundPositioning filename =
    case filename of
        "2.jpg" ->
            style "background-position" "center 68%"

        "3.jpg" ->
            style "background-position" "center 30%"

        "4.jpg" ->
            style "background-position" "center 96.125%"

        "6.jpg" ->
            style "background-position" "center 40%"

        "11.jpg" ->
            style "background-position" "center 67.25%"

        "19.jpg" ->
            style "background-position" "center 13%"

        "20.jpg" ->
            style "background-position" "center 39.75%"

        "21.jpg" ->
            style "background-position" "center 52.5%"

        "22.jpg" ->
            style "background-position" "center top"

        "23.jpg" ->
            style "background-position" "center 92.5%"

        "24.jpg" ->
            style "background-position" "center top"

        "25.jpg" ->
            style "background-position" "center 50%"

        "27.jpg" ->
            style "background-position" "center top"

        _ ->
            style "background-position" "center bottom"



-----------------------------------------
-- ㊙️
-----------------------------------------


chosen : Maybe String -> Html Msg
chosen maybeChosen =
    case maybeChosen of
        Just c ->
            let
                loadingDecoder =
                    c
                        |> LoadBackdrop
                        |> Json.Decode.succeed
            in
            slab
                Html.img
                [ on "load" loadingDecoder
                , src ("images/Background/" ++ c)
                , style "opacity" "0.00001"
                ]
                [ "fixed"
                , "h-px"
                , "left-full"
                , "overflow-hidden"
                , "top-full"
                , "w-px"
                ]
                []

        Nothing ->
            nothing


loaded : List String -> Bool -> Html Msg
loaded list fadeIn =
    let
        amount =
            List.length list

        indexedMapFn idx =
            image fadeIn (idx + 1 < amount)
    in
    list
        |> List.indexedMap indexedMapFn
        |> raw


image : Bool -> Bool -> String -> Html msg
image fadeIn isPrevious loadedBackdrop =
    let
        defaultClasses =
            [ "bg-cover"
            , "fixed"
            , "inset-0"

            -- Opacity
            ----------
            , if isPrevious || not fadeIn then
                "opacity-100"

              else
                "opacity-0"
            ]

        animationClasses =
            if not isPrevious && fadeIn then
                [ "animation-2s"
                , "animation-delay-50ms"
                , "animation-fadeIn"
                , "animation-fill-forwards"
                , "animation-once"
                ]

            else
                []
    in
    brick
        [ backgroundImage loadedBackdrop
        , backgroundPositioning loadedBackdrop
        ]
        (List.append
            defaultClasses
            animationClasses
        )
        []
