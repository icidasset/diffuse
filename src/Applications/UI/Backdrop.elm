module UI.Backdrop exposing (Model, Msg(..), initialModel, update, view)

import Html exposing (Html, div)
import Html.Attributes exposing (src, style)
import Html.Events exposing (on)
import Html.Lazy
import Json.Decode
import Replying exposing (R3D3)
import Return3 as Return
import Tachyons
import Tachyons.Classes as Tachyons
import UI.Reply as Reply exposing (Reply)



-- ⛩


default : String
default =
    "7.jpg"



-- 🌳


type alias Model =
    { chosen : String
    , fadeIn : Bool
    , loaded : List String
    }


initialModel : Model
initialModel =
    { chosen = default
    , fadeIn = True
    , loaded = []
    }



-- 📣


type Msg
    = Load String


update : Msg -> Model -> R3D3 Model Msg Reply
update msg model =
    case msg of
        Load backdrop ->
            [ backdrop ]
                |> List.append model.loaded
                |> (\list -> { model | loaded = list })
                |> Return.withNothing



-- 🗺


view : Model -> Html Msg
view model =
    div
        [ Tachyons.classes
            [ Tachyons.absolute__fill
            , Tachyons.fixed
            , Tachyons.z_0
            ]
        ]
        [ Html.Lazy.lazy chosen model.chosen
        , Html.Lazy.lazy2 loaded model.loaded model.fadeIn
        ]



-----------------------------------------
-- Private
-----------------------------------------


chosen : String -> Html Msg
chosen c =
    let
        loadingDecoder =
            c
                |> Load
                |> Json.Decode.succeed
    in
    Html.img
        [ style "height" "1px"
        , style "left" "100%"
        , style "opacity" "0.00001"
        , style "overflow" "hidden"
        , style "position" "fixed"
        , style "top" "100%"
        , style "transform" "translate(-1px, -1px)"
        , style "width" "1px"
        , style "z-index" "-10000"

        --
        , src ("/images/Background/" ++ c)
        , on "load" loadingDecoder
        ]
        []


loaded : List String -> Bool -> Html Msg
loaded list fadeIn =
    let
        amount =
            min (List.length list) 2

        indexedMapFn idx item =
            div
                (imageStyles fadeIn (idx + 1 < amount) item)
                []
    in
    list
        |> List.reverse
        |> List.take 2
        |> List.reverse
        |> List.indexedMap indexedMapFn
        |> div []


imageStyles : Bool -> Bool -> String -> List (Html.Attribute Msg)
imageStyles fadeIn isPrevious loadedBackdrop =
    [ style
        "animation"
        (if not isPrevious && fadeIn then
            "2s ease-in 50ms forwards fadeIn"

         else
            "none"
        )

    --
    , style
        "opacity"
        (if isPrevious || not fadeIn then
            "1"

         else
            "0"
        )

    --
    , style "background-image" ("url(/images/Background/" ++ loadedBackdrop ++ ")")
    , style "background-size" "cover"
    , style "bottom" "-1px"
    , style "left" "-1px"
    , style "position" "fixed"
    , style "right" "-1px"
    , style "top" "-1px"
    , style "z-index" "-9"

    --
    , case loadedBackdrop of
        "1.jpg" ->
            style "background-position" "center 30%"

        "9.jpg" ->
            style "background-position" "center 68%"

        _ ->
            style "background-position" "center bottom"
    ]
