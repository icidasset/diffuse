module UI.Backdrop exposing (Model, Msg(..), initialModel, update, view)

import Chunky exposing (..)
import Css exposing (..)
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (css, src, style)
import Html.Styled.Events exposing (on)
import Html.Styled.Lazy as Lazy
import Json.Decode
import Replying exposing (R3D3)
import Return3 as Return
import Tachyons.Classes as T
import UI.Animations
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
    chunk
        [ T.absolute__fill
        , T.fixed
        , T.z_0
        ]
        [ Lazy.lazy chosen model.chosen
        , Lazy.lazy2 loaded model.loaded model.fadeIn
        ]



-----------------------------------------
-- ㊙️
-----------------------------------------


chosen : String -> Html Msg
chosen c =
    let
        loadingDecoder =
            c
                |> Load
                |> Json.Decode.succeed
    in
    slab
        Html.img
        [ css chosenStyles
        , on "load" loadingDecoder
        , src ("/images/Background/" ++ c)
        ]
        [ T.fixed
        , T.overflow_hidden
        ]
        []


loaded : List String -> Bool -> Html Msg
loaded list fadeIn =
    let
        amount =
            min (List.length list) 2

        indexedMapFn idx item =
            div [ css (imageStyles fadeIn (idx + 1 < amount) item) ] []
    in
    list
        |> List.reverse
        |> List.take 2
        |> List.reverse
        |> List.indexedMap indexedMapFn
        |> div []



-- 🖼


chosenStyles : List Css.Style
chosenStyles =
    [ height (px 1)
    , left (pct 100)
    , opacity (num 0.00001)
    , top (pct 100)
    , transform (translate2 (px -1) (px -1))
    , width (px 1)
    , zIndex (int -10000)
    ]


imageAnimation : List Css.Style
imageAnimation =
    [ animationName UI.Animations.fadeIn
    , animationDuration (ms 2000)
    , animationDelay (ms 50)
    , property "animation-fill-mode" "forwards"
    ]


imageStyles : Bool -> Bool -> String -> List Css.Style
imageStyles fadeIn isPrevious loadedBackdrop =
    List.append
        -- Animation
        ------------
        (if not isPrevious && fadeIn then
            imageAnimation

         else
            []
        )
        [ -- Background
          -------------
          case loadedBackdrop of
            "1.jpg" ->
                property "background-position" "center 30%"

            "9.jpg" ->
                property "background-position" "center 68%"

            _ ->
                property "background-position" "center bottom"

        -- Opacity
        ----------
        , if isPrevious || not fadeIn then
            opacity (int 1)

          else
            opacity zero

        --
        , backgroundImage (url <| "/images/Background/" ++ loadedBackdrop)
        , backgroundSize cover
        , bottom (px -1)
        , left (px -1)
        , position fixed
        , right (px -1)
        , top (px -1)
        , zIndex (int -9)
        ]
