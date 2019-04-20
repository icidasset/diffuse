module UI.Backdrop exposing (Model, Msg(..), default, initialModel, options, update, view)

import Chunky exposing (..)
import Css exposing (..)
import Css.Global
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (class, css, src, style)
import Html.Styled.Events exposing (on)
import Html.Styled.Lazy as Lazy
import Json.Decode
import Return3 as Return exposing (..)
import Tachyons.Classes as T
import UI.Animations
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
    ]



-- 🌳


type alias Model =
    { chosen : Maybe String
    , fadeIn : Bool
    , loaded : List String
    }


initialModel : Model
initialModel =
    { chosen = Nothing
    , fadeIn = True
    , loaded = []
    }



-- 📣


type Msg
    = Choose String
    | Default
    | Load String


update : Msg -> Model -> Return Model Msg Reply
update msg model =
    case msg of
        Choose backdrop ->
            return { model | chosen = Just backdrop } |> addReply Reply.SaveSettings

        Default ->
            return { model | chosen = Just default }

        Load backdrop ->
            return { model | loaded = model.loaded ++ [ backdrop ] }



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
                [ css chosenStyles
                , on "load" loadingDecoder
                , src ("/images/Background/" ++ c)
                ]
                [ T.fixed
                , T.overflow_hidden
                ]
                []

        Nothing ->
            nothing


loaded : List String -> Bool -> Html Msg
loaded list fadeIn =
    let
        amount =
            List.length list

        indexedMapFn idx item =
            div (imageStyles fadeIn (idx + 1 < amount) item) []
    in
    list
        |> List.indexedMap indexedMapFn
        |> div [ css imageContainerStyles ]



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


imageContainerStyles : List Css.Style
imageContainerStyles =
    [ Css.Global.descendants
        [ Css.Global.selector
            ".bg-image--with-fadein"
            imageAnimation
        ]
    ]


imageAnimation : List Css.Style
imageAnimation =
    [ animationName UI.Animations.fadeIn
    , animationDuration (ms 2000)
    , animationDelay (ms 50)
    , property "animation-fill-mode" "forwards"
    ]


imageStyles : Bool -> Bool -> String -> List (Html.Attribute msg)
imageStyles fadeIn isPrevious loadedBackdrop =
    [ -- Animation
      ------------
      if not isPrevious && fadeIn then
        class "bg-image--with-fadein"

      else
        class ""

    -- Background
    -------------
    , case loadedBackdrop of
        "1.jpg" ->
            style "background-position" "center 30%"

        "9.jpg" ->
            style "background-position" "center 68%"

        _ ->
            style "background-position" "center bottom"

    -- Opacity
    ----------
    , if isPrevious || not fadeIn then
        style "opacity" "1"

      else
        style "opacity" "0"

    --
    , style "background-image" ("url(/images/Background/" ++ loadedBackdrop ++ ")")
    , style "background-size" "cover"
    , style "bottom" "-1px"
    , style "left" "-1px"
    , style "position" "fixed"
    , style "right" "-1px"
    , style "top" "-1px"
    , style "z-index" "-9"
    ]
