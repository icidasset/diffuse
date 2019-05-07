module UI.DnD exposing (Environment, Model, Msg, environmentSubject, environmentTarget, hasDropped, initialModel, isBeingDraggedOver, listenToDrop, listenToEnterLeave, listenToStart, modelSubject, modelTarget, stoppedDragging, update)

import Html.Events.Extra.Pointer as Pointer
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attributes
import UI.Reply as Reply exposing (Reply)



-- 🌳


type Model context
    = NotDragging
    | Dragging { subject : context }
    | DraggingOver { subject : context, target : context }
    | Dropped { subject : context, target : context }


type Msg context
    = Start context
    | Enter context
    | Leave context
    | Drop context
    | Stop


type alias Environment context msg =
    { model : Model context
    , toMsg : Msg context -> msg
    }


initialModel : Model context
initialModel =
    NotDragging



-- 📣


update : Msg context -> Model context -> ( Model context, List Reply )
update msg model =
    ( ------------------------------------
      -- Model
      ------------------------------------
      case msg of
        Start context ->
            Dragging { subject = context }

        Enter context ->
            case model of
                Dragging { subject } ->
                    DraggingOver { subject = subject, target = context }

                _ ->
                    model

        Leave _ ->
            case model of
                DraggingOver { subject } ->
                    Dragging { subject = subject }

                _ ->
                    model

        Drop context ->
            case model of
                DraggingOver { subject } ->
                    Dropped { subject = subject, target = context }

                _ ->
                    NotDragging

        Stop ->
            NotDragging
      ------------------------------------
      -- Reply
      ------------------------------------
    , case msg of
        Start context ->
            [ Reply.StartedDragging ]

        _ ->
            []
    )



-- 🔱  ░░  EVENTS & MESSAGES


listenToStart : Environment context msg -> context -> Attribute msg
listenToStart { toMsg } context =
    context
        |> Start
        |> toMsg
        |> always
        |> Pointer.onDown
        |> Attributes.fromUnstyled


listenToEnterLeave : Environment context msg -> context -> List (Attribute msg)
listenToEnterLeave { model, toMsg } context =
    case model of
        NotDragging ->
            []

        _ ->
            [ context
                |> Enter
                |> toMsg
                |> always
                |> Pointer.onEnter
                |> Attributes.fromUnstyled
            , context
                |> Leave
                |> toMsg
                |> always
                |> Pointer.onLeave
                |> Attributes.fromUnstyled
            ]


listenToDrop : Environment context msg -> context -> List (Attribute msg)
listenToDrop { model, toMsg } context =
    case model of
        NotDragging ->
            []

        _ ->
            [ context
                |> Drop
                |> toMsg
                |> always
                |> Pointer.onUp
                |> Attributes.fromUnstyled
            ]


stoppedDragging : Msg context
stoppedDragging =
    Stop



-- 🔱  ░░  MODEL


isBeingDraggedOver : context -> Model context -> Bool
isBeingDraggedOver context model =
    case model of
        DraggingOver { target } ->
            context == target

        _ ->
            False


hasDropped : Model context -> Bool
hasDropped model =
    case model of
        Dropped _ ->
            True

        _ ->
            False


modelSubject : Model context -> Maybe context
modelSubject model =
    case model of
        NotDragging ->
            Nothing

        Dragging { subject } ->
            Just subject

        DraggingOver { subject } ->
            Just subject

        Dropped { subject } ->
            Just subject


modelTarget : Model context -> Maybe context
modelTarget model =
    case model of
        NotDragging ->
            Nothing

        Dragging _ ->
            Nothing

        DraggingOver { target } ->
            Just target

        Dropped { target } ->
            Just target



-- 🔱  ░░  ENVIRONMENT


environmentSubject : Environment context msg -> Maybe context
environmentSubject =
    .model >> modelSubject


environmentTarget : Environment context msg -> Maybe context
environmentTarget =
    .model >> modelTarget
