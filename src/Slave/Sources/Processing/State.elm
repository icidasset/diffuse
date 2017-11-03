module Sources.Processing.State exposing (update)

import Date
import Json.Encode as Encode
import Slave.Events exposing (..)
import Sources.Processing.Ports as Ports
import Sources.Processing.Steps as Steps
import Sources.Processing.Utils exposing (..)
import Slave.Types as Slave
import Tracks.Encoding


-- 💧


initialModel : Model
initialModel =
    { errors = []
    , status = Nothing
    , timestamp = Date.fromTime 0
    }



-- 🔥


update : Msg -> Model -> ( Model, Cmd Slave.Msg )
update msg model =
    case msg of
        {- If already processing, do nothing.
           If there are no sources, do nothing.
           If there are sources, start processing the first source.
        -}
        Process sources tracks ->
            let
                processingData =
                    List.map
                        (\source ->
                            ( source
                            , List.filter (\t -> t.sourceId == source.id) tracks
                            )
                        )
                        sources

                status =
                    sources
                        |> List.head
                        |> Maybe.map (always processingData)
                        |> Maybe.preferFirst model.status

                command =
                    sources
                        |> List.head
                        |> Maybe.map (Steps.takeFirstStep model.timestamp)
                        |> Maybe.preferSecond (Maybe.map (always Cmd.none) model.status)
                        |> Maybe.withDefault Cmd.none

                errors =
                    model.status
                        |> Maybe.map (\_ -> model.errors)
                        |> Maybe.withDefault []
            in
                ($)
                    { model | status = status, errors = errors }
                    [ command ]
                    []

        {- If not processing, do nothing.
           If there are no sources left, do nothing.
           If there are sources left, start processing the next source in line.
        -}
        NextInLine ->
            let
                takeStep =
                    Steps.takeFirstStep model.timestamp

                maybe =
                    model.status
                        |> Maybe.andThen (List.tail)
                        |> Maybe.andThen (\a -> Maybe.map ((,) a) (List.head a))
                        |> Maybe.map (Tuple.mapSecond Tuple.first)
                        |> Maybe.map (Tuple.mapSecond takeStep)
            in
                ($)
                    { model | status = Maybe.map Tuple.first maybe }
                    [ maybe
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault Cmd.none
                    ]
                    []

        {- Phase 1, `makeTree`.
           ie. make a file list/tree.
        -}
        TreeStep context (Ok response) ->
            let
                associatedTracks =
                    model.status
                        |> Maybe.andThen List.head
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault []
            in
                ($)
                    model
                    [ Steps.takeTreeStep context response associatedTracks model.timestamp ]
                    []

        --
        -- Error
        --
        TreeStep context (Err err) ->
            let
                publicError =
                    case err of
                        NetworkError ->
                            "Cannot connect to this source"

                        Timeout ->
                            "Source did not respond (timeout)"

                        BadStatus response ->
                            Services.parseErrorResponse context.source.service response.body

                        _ ->
                            toString err
            in
                ($)
                    { model
                        | errors =
                            ( ctx.source.id, publicError ) :: model.errors
                    }
                    [ do NextInLine ]
                    []

        --
        -- Remove tracks
        --
        TreeStepRemoveTracks sourceId filePaths ->
            let
                encodedFilePaths =
                    filePaths
                        |> List.map Encode.string
                        |> Encode.list

                encodedSourceId =
                    Encode.string sourceId

                encodedData =
                    Encode.object
                        [ ( "filePaths", encodedFilePaths )
                        , ( "sourceId", encodedSourceId )
                        ]
            in
                (!) model [ issueWithData RemoveTracksByPath encodedData ]

        {- Phase 2, `makeTags`.
           ie. get the tags for each file in the file list.
        -}
        TagsStep tagsContext ->
            let
                insert =
                    issueWithData
                        AddTracks
                        (tagsContext
                            |> Steps.tracksFromTagsContext
                            |> List.map Tracks.Encoding.encodeTrack
                            |> Encode.list
                        )

                cmd =
                    model.isProcessing
                        |> Maybe.map (List.map Tuple.first)
                        |> Maybe.andThen (Steps.findTagsContextSource tagsContext)
                        |> Maybe.andThen (Steps.takeTagsStep model.timestamp tagsContext)
                        |> Maybe.withDefault (do NextInLine)
            in
                ($) model [ cmd ] [ insert ]



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.receiveTags TagsStep ]
