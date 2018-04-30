module DnD exposing (..)

import Html
import Html.Attributes exposing (attribute)
import Html.Events exposing (on)
import Json.Decode as Decode


-- ⚗️


type Msg subject
    = Drop subject
    | End
    | Out subject
    | Over subject
    | Start subject


type Model subject
    = Dragging subject
    | DraggedOver { origin : subject, target : subject }
    | Dropped { origin : subject, target : subject }
    | NotDragging



-- 🎒


itemHooks : (Msg subject -> wrap) -> subject -> List (Html.Attribute wrap)
itemHooks wrap subject =
    [ on "mousedown" (whenLeftMouseButton <| wrap <| Start subject)
    , on "longtap" (Decode.succeed <| wrap <| Start subject)

    --
    , on "pointerup" (Decode.succeed <| wrap <| Drop subject)
    , on "pointerout" (Decode.succeed <| wrap <| Out subject)
    , on "pointerover" (Decode.succeed <| wrap <| Over subject)

    --
    , attribute "draggable" "true"
    , attribute "ondragstart" "event.preventDefault();"
    ]


containerHooks : (Msg subject -> wrap) -> List (Html.Attribute wrap)
containerHooks wrap =
    [ on "pointerup" (Decode.succeed <| wrap <| End)
    ]


whenLeftMouseButton : msg -> Decode.Decoder msg
whenLeftMouseButton msg =
    let
        -- See the following link for the integer values:
        -- https://developer.mozilla.org/en-US/docs/Web/Events/mousedown
        buttonHandler int =
            case int of
                1 ->
                    Decode.succeed msg

                _ ->
                    Decode.fail "The left-mouse button was not used"
    in
        Decode.int
            |> Decode.field "buttons"
            |> Decode.andThen buttonHandler



-- 💧


initial : Model subject
initial =
    NotDragging



-- 🔥


update : Msg subject -> Model subject -> Model subject
update msg model =
    case msg of
        ------------------------------------
        -- 1
        ------------------------------------
        Start subject ->
            Dragging subject

        ------------------------------------
        -- 2
        ------------------------------------
        Over subject ->
            case model of
                Dragging origin ->
                    DraggedOver { origin = origin, target = subject }

                _ ->
                    model

        ------------------------------------
        -- 3.A
        ------------------------------------
        Out subject ->
            case model of
                Dragging origin ->
                    model

                DraggedOver { origin } ->
                    Dragging origin

                _ ->
                    NotDragging

        ------------------------------------
        -- 3.B
        ------------------------------------
        Drop subject ->
            Dropped
                { origin = Maybe.withDefault subject (dragSubject model)
                , target = subject
                }

        ------------------------------------
        -- 4
        ------------------------------------
        End ->
            NotDragging



-- 🌱


dragSubject : Model subject -> Maybe subject
dragSubject model =
    case model of
        Dragging subject ->
            Just subject

        DraggedOver { origin } ->
            Just origin

        Dropped { origin } ->
            Just origin

        NotDragging ->
            Nothing


overSubject : Model subject -> Maybe subject
overSubject model =
    case model of
        DraggedOver { origin, target } ->
            Just target

        _ ->
            Nothing
