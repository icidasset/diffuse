module Brain.Sources.Processing exposing (initialCommand, initialModel, subscriptions, update)

import Alien
import Brain.Ports
import Brain.Reply exposing (Reply(..))
import Brain.Sources.Processing.Common exposing (..)
import Brain.Sources.Processing.Steps as Steps
import Json.Encode as Encode
import List.Extra as List
import Maybe.Extra as Maybe
import Return3 as Return exposing (..)
import Sources.Processing exposing (..)
import Task
import Task.Extra exposing (do)
import Time
import Time.Ext as Time
import Tracks.Encoding



-- 🌳


initialModel : Model
initialModel =
    { currentTime = Time.default
    , origin = "ORIGIN_UNKNOWN"
    , status = NotProcessing
    }


initialCommand : Cmd Msg
initialCommand =
    Task.perform SetCurrentTime Time.now



-- 📣


update : Msg -> Model -> Return Model Msg Reply
update msg model =
    case msg of
        {- If already processing, do nothing.
           If there are no sources, do nothing.
           If there are sources, start processing the first source.
        -}
        Process { origin, sources, tracks } ->
            let
                filter s =
                    List.filter (.sourceId >> (==) s.id) tracks

                all =
                    List.map (\s -> ( s, filter s )) sources
            in
            case
                ( isProcessing model.status || List.isEmpty sources
                , List.uncons all
                )
            of
                ( False, Just ( ( s, t ), future ) ) ->
                    Return.commandWithModel
                        { model | origin = origin, status = Processing ( s, t ) future }
                        (Steps.takeFirstStep origin model.currentTime s)

                _ ->
                    return model

        {- If not processing, do nothing.
           If there are no sources left, do nothing.
           If there are sources left, start processing the next source in line.
        -}
        NextInLine ->
            case model.status of
                Processing _ (( source, tracks ) :: rest) ->
                    Return.commandWithModel
                        { model | status = Processing ( source, tracks ) rest }
                        (Steps.takeFirstStep model.origin model.currentTime source)

                _ ->
                    Return.repliesWithModel
                        { model | status = NotProcessing }
                        [ NudgeUI Alien.FinishedProcessingSources ]

        -----------------------------------------
        -- Phase 1
        -- Prepare for processing.
        -----------------------------------------
        PrepareStep context (Ok response) ->
            let
                ( cmd, replies ) =
                    Steps.takePrepareStep context response model.currentTime
            in
            ( model
            , cmd
            , replies
            )

        PrepareStep context (Err err) ->
            ( model
            , do NextInLine
            , [ reportHttpError context.source err ]
            )

        -----------------------------------------
        -- Phase 2
        -- Make a file list/tree.
        -----------------------------------------
        TreeStep context (Ok response) ->
            case model.status of
                Processing ( source, tracks ) rest ->
                    ( { model | status = Processing ( context.source, tracks ) rest }
                    , Steps.takeTreeStep context response tracks model.currentTime
                    , []
                    )

                NotProcessing ->
                    return model

        TreeStep context (Err err) ->
            ( model
            , do NextInLine
            , [ reportHttpError context.source err ]
            )

        TreeStepRemoveTracks sourceId filePaths ->
            let
                encodedData =
                    Encode.object
                        [ ( "filePaths", Encode.list Encode.string filePaths )
                        , ( "sourceId", Encode.string sourceId )
                        ]
            in
            ( model
            , Cmd.none
            , [ GiveUI Alien.RemoveTracksByPath encodedData ]
            )

        -----------------------------------------
        -- Phase 3
        -- Get the tags for each file in the file list.
        -----------------------------------------
        TagsStep tagsContext ->
            ( model
              ----------
              -- Command
              ----------
            , case model.status of
                Processing ( source, _ ) _ ->
                    source
                        |> Steps.takeTagsStep model.currentTime tagsContext
                        |> Maybe.withDefault (do NextInLine)

                NotProcessing ->
                    Cmd.none
              --------
              -- Reply
              --------
            , case List.isEmpty (List.filter Maybe.isJust tagsContext.receivedTags) of
                True ->
                    []

                False ->
                    tagsContext
                        |> tracksFromTagsContext
                        |> List.map (\track -> { track | insertedAt = model.currentTime })
                        |> Encode.list Tracks.Encoding.encodeTrack
                        |> GiveUI Alien.AddTracks
                        |> List.singleton
            )

        -----------------------------------------
        -- Bits & Pieces
        -----------------------------------------
        SetCurrentTime time ->
            return { model | currentTime = time }



-- 📰


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every (60 * 1000) SetCurrentTime
        , Brain.Ports.receiveTags TagsStep
        ]
