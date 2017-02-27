module Sources.State exposing (..)

import Date
import Debug
import Navigation
import Sources.Processing as Processing
import Sources.Types exposing (..)
import Task


-- Services

import Sources.Services.AmazonS3 as AmazonS3


-- 💧


initialModel : Model
initialModel =
    { isProcessing = Nothing
    , newSource = AmazonS3 AmazonS3.initialProperties
    , sources = []
    , timestamp = Date.fromTime 0
    }


initialCommands : Cmd Msg
initialCommands =
    Cmd.none



-- 🔥


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ------------------------------------
        -- Process
        ------------------------------------
        {- If already processing, keep doing as you were.
           If there are no sources, do nothing.
           If there are sources, start processing the first source.
        -}
        Process ->
            let
                isProcessing =
                    case model.isProcessing of
                        Just _ ->
                            model.isProcessing

                        Nothing ->
                            if List.length model.sources > 0 then
                                Just model.sources
                            else
                                Nothing

                command =
                    case model.isProcessing of
                        Just _ ->
                            Cmd.none

                        Nothing ->
                            case List.head model.sources of
                                Just source ->
                                    Processing.takeFirstStep source model.timestamp

                                Nothing ->
                                    Cmd.none
            in
                (!)
                    { model | isProcessing = isProcessing }
                    [ command ]

        {- Processing step,
           Phase 1, `makeTree`.
        -}
        ProcessStep context (Ok stringResponse) ->
            let
                newContext =
                    Processing.takeNextStep context stringResponse

                command =
                    case newContext.treeMarker of
                        InProgress _ ->
                            Processing.process newContext model.timestamp

                        _ ->
                            -- TheEnd
                            -- TODO
                            let
                                zIsDead =
                                    Debug.log "Finished with the first!" context
                            in
                                Cmd.none
            in
                (!) model [ command ]

        ProcessStep _ (Err err) ->
            -- TODO: Handle Http.Error
            let
                e_ =
                    Debug.log "Error" (toString err)
            in
                (!) model []

        ------------------------------------
        -- Forms
        ------------------------------------
        SetNewSource source ->
            (!) { model | newSource = source } []

        SetNewSourceProperty source propKey propValue ->
            let
                updatedSource =
                    case source of
                        AmazonS3 sourceData ->
                            propValue
                                |> AmazonS3.translateTo sourceData propKey
                                |> AmazonS3
            in
                (!) { model | newSource = updatedSource } []

        SubmitNewSourceForm ->
            (!)
                { model | sources = model.newSource :: model.sources }
                [ Task.perform identity (Task.succeed Process) ]
