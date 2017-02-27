module Sources.Processing exposing (..)

import Date exposing (Date)
import Sources.Types exposing (..)


-- Services

import Sources.Services.AmazonS3 as AmazonS3


-- Processor


process : ProcessingContext -> (Date -> Cmd Msg)
process context =
    let
        msg =
            ProcessStep context
    in
        case context.source of
            AmazonS3 sourceData ->
                AmazonS3.makeTree msg sourceData context.treeMarker



-- Steps


takeFirstStep : Source -> (Date -> Cmd Msg)
takeFirstStep source =
    let
        initialContext =
            { filePaths = []
            , source = source
            , treeMarker = TheBeginning
            }
    in
        process initialContext


takeNextStep : ProcessingContext -> String -> ProcessingContext
takeNextStep context response =
    case context.source of
        AmazonS3 _ ->
            AmazonS3.handleTreeResponse context response
