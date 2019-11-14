module UI.Backdrop exposing (Model, Msg(..), backgroundPositioning, default, initialModel, options, update, view)

import Chunky exposing (..)
import Color exposing (Color)
import Css exposing (int, num, pct, px)
import Css.Classes as C
import Css.Global
import Html exposing (Html, div)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (on)
import Html.Lazy as Lazy
import Json.Decode
import Return3 exposing (..)
import UI.Ports as Ports
import UI.Reply as Reply exposing (Reply)



-- ⛩


default : String
default =
    "7.jpg"


options : List ( String, String )
options =
    [ ( "1.jpg", "Option 1" )
    , ( "2.jpg", "Option 2" )
    , ( "3.jpg", "Option 3" )
    , ( "4.jpg", "Option 4" )
    , ( "5.jpg", "Option 5" )
    , ( "6.jpg", "Option 6" )
    , ( "7.jpg", "Option 7 (default)" )
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
    , ( "21.jpg", "Option 21" )
    ]



-- 🌳


type alias Model =
    { bgColor : Maybe Color
    , chosen : Maybe String
    , fadeIn : Bool
    , loaded : List String
    }


initialModel : Model
initialModel =
    { bgColor = Nothing
    , chosen = Nothing
    , fadeIn = True
    , loaded = []
    }



-- 📣


type Msg
    = BackgroundColor { r : Int, g : Int, b : Int }
    | Choose String
    | Default
    | Load String


update : Msg -> Model -> Return Model Msg Reply
update msg model =
    case msg of
        BackgroundColor { r, g, b } ->
            return { model | bgColor = Just (Color.rgb255 r g b) }

        Choose backdrop ->
            return { model | chosen = Just backdrop } |> addReply Reply.SaveSettings

        Default ->
            return { model | chosen = Just default }

        Load backdrop ->
            returnCommandWithModel
                { model | loaded = model.loaded ++ [ backdrop ] }
                (Ports.pickAverageBackgroundColor backdrop)



-- 🗺


view : Model -> Html Msg
view model =
    chunk
        [ C.fixed
        , C.inset_0
        , C.z_0
        ]
        [ Lazy.lazy chosen model.chosen
        , Lazy.lazy2 loaded model.loaded model.fadeIn

        -- Shadow
        ---------
        , brick
            [ style "background" "linear-gradient(#0000, rgba(0, 0, 0, 0.175))" ]
            [ C.absolute
            , C.bottom_0
            , C.h_64
            , C.inset_x_0
            , C.z_10
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

        "17.jpg" ->
            style "background-position" "center 87.5%"

        "19.jpg" ->
            style "background-position" "center 13%"

        "20.jpg" ->
            style "background-position" "center 39.75%"

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
                        |> Load
                        |> Json.Decode.succeed
            in
            slab
                Html.img
                [ on "load" loadingDecoder
                , src ("images/Background/" ++ c)
                , style "opacity" "0.00001"
                ]
                [ C.fixed
                , C.h_px
                , C.left_full
                , C.overflow_hidden
                , C.top_full
                , C.w_px
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
            [ C.bg_cover
            , C.fixed
            , C.inset_0

            -- Opacity
            ----------
            , if isPrevious || not fadeIn then
                C.opacity_100

              else
                C.opacity_0
            ]

        animationClasses =
            if not isPrevious && fadeIn then
                [ C.animation_2s
                , C.animation_delay_50ms
                , C.animation_fadeIn
                , C.animation_fill_forwards
                , C.animation_once
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
