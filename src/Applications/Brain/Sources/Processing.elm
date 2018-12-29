module Brain.Sources.Processing exposing (initialCommand, initialModel, subscriptions, update)

import Brain.Reply exposing (Reply(..))
import Brain.Sources.Processing.Common exposing (..)
import Brain.Sources.Processing.Steps as Steps
import Http
import Replying exposing (R3D3)
import Return3
import Sources.Processing exposing (..)
import Time



-- 🌳


initialModel : Model
initialModel =
    { currentTime = Time.millisToPosix 0
    , origin = "ORIGIN_UNKNOWN"
    , status = NotProcessing
    }


initialCommand : Cmd Msg
initialCommand =
    Cmd.none



-- 📣


update : Msg -> Model -> R3D3 Model Msg Reply
update msg model =
    case msg of
        {- If already processing, do nothing.
           If there are no sources, do nothing.
           If there are sources, start processing the first source.
        -}
        Process { origin, sources, tracks } ->
            if isProcessing model.status then
                Return3.withNothing model

            else
                case List.head sources of
                    Just source ->
                        let
                            filter s =
                                List.filter (.sourceId >> (==) s.id) tracks

                            status =
                                sources
                                    |> List.map (\s -> ( s, filter s ))
                                    |> Processing
                        in
                        ( { model | status = status }
                        , Steps.takeFirstStep origin model.currentTime source
                        , Nothing
                        )

                    Nothing ->
                        Return3.withNothing model

        {- If not processing, do nothing.
           If there are no sources left, do nothing.
           If there are sources left, start processing the next source in line.
        -}
        NextInLine ->
            ( model
            , Cmd.none
            , Nothing
            )

        -----------------------------------------
        -- Phase 1
        -- Prepare for processing.
        -----------------------------------------
        PrepareStep context (Ok response) ->
            ( model
            , Cmd.none
            , Nothing
            )

        PrepareStep context (Err err) ->
            ( model
            , Cmd.none
            , Just [ reportHttpError context.source err ]
            )

        -----------------------------------------
        -- Phase 2
        -- Make a file list/tree.
        -----------------------------------------
        TreeStep context (Ok response) ->
            ( model
            , Cmd.none
            , Nothing
            )

        TreeStep context (Err err) ->
            ( model
            , Cmd.none
            , Just [ reportHttpError context.source err ]
            )

        TreeStepRemoveTracks sourceId filePaths ->
            -- TODO
            ( model
            , Cmd.none
            , Nothing
            )

        -----------------------------------------
        -- Phase 3
        -- Get the tags for each file in the file list.
        -----------------------------------------
        TagsStep tagsContext ->
            ( model
            , Cmd.none
            , Nothing
            )

        -----------------------------------------
        -- Bits & Pieces
        -----------------------------------------
        SetCurrentTime time ->
            ( { model | currentTime = time }
            , Cmd.none
            , Nothing
            )



-- 📰


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (60 * 1000) SetCurrentTime
