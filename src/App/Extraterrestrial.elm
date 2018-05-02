module Extraterrestrial exposing (..)

{-| Functions to help handling alien events.
-}

import Dict
import Dict.Ext as Dict
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode
import Response.Ext as Response exposing (do)
import Tracks.Encoding
import Tracks.Types
import Types as TopLevel exposing (Msg(TracksMsg))
import Utils exposing (displayError)


type alias Response =
    ( TopLevel.Model, Cmd TopLevel.Msg )



-- Sources


processSourcesCompleted : TopLevel.Model -> Response
processSourcesCompleted model =
    model.sources
        |> (\s -> { model | sources = { s | isProcessing = Nothing } })
        |> (\m -> ( m, Cmd.none ))


reportProcessingError : TopLevel.Model -> Encode.Value -> Response
reportProcessingError model val =
    let
        err =
            Result.withDefault
                Dict.empty
                (decodeValue (dict string) val)

        errors =
            (::)
                ( Dict.fetch "sourceId" "BEEP" err, Dict.fetch "message" "BOOP" err )
                model.sources.processingErrors
    in
        model.sources
            |> (\s -> { model | sources = { s | processingErrors = errors } })
            |> (\m -> ( m, Cmd.none ))



-- Tracks


addTracks : TopLevel.Model -> Encode.Value -> Response
addTracks model val =
    (!)
        model
        [ case decodeValue (Decode.list Tracks.Encoding.trackDecoder) val of
            Ok tracks ->
                tracks
                    |> Tracks.Types.Add
                    |> TracksMsg
                    |> do

            Err err ->
                displayError err
        ]


removeTracksByPath : TopLevel.Model -> Encode.Value -> Response
removeTracksByPath model val =
    (!)
        model
        [ case decodeValue (dict value) val of
            Ok dictionary ->
                let
                    filePaths =
                        dictionary
                            |> Dict.fetch "filePaths" (Encode.list [])
                            |> Decode.decodeValue (list string)
                            |> Result.withDefault []

                    nonExistantSid =
                        "BEEP_BOOP"

                    sourceId =
                        dictionary
                            |> Dict.fetch "sourceId" (Encode.string nonExistantSid)
                            |> Decode.decodeValue string
                            |> Result.withDefault nonExistantSid
                in
                    filePaths
                        |> Tracks.Types.RemoveByPath sourceId
                        |> TracksMsg
                        |> do

            Err err ->
                displayError err
        ]
