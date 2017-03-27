module Sources.Processing
    exposing
        ( takeFirstStep
        , takeTreeStep
        , takeTagsStep
        )

import Date exposing (Date)
import Sources.Types exposing (..)


-- Services

import Sources.Services.AmazonS3 as AmazonS3


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
        makeTree initialContext


takeTreeStep : ProcessingContext -> String -> ( ProcessingContext, Maybe (Date -> Cmd Msg) )
takeTreeStep context response =
    let
        newContext =
            handleTreeResponse context response
    in
        case context.treeMarker of
            InProgress _ ->
                ( newContext
                , Just (makeTree context)
                )

            _ ->
                ( newContext
                , Nothing
                )


takeTagsStep : ProcessingContext -> Cmd Msg
takeTagsStep _ =
    Cmd.none



-- Tree


handleTreeResponse : ProcessingContext -> String -> ProcessingContext
handleTreeResponse context response =
    case context.source of
        AmazonS3 _ ->
            AmazonS3.handleTreeResponse context response


makeTree : ProcessingContext -> (Date -> Cmd Msg)
makeTree context =
    let
        msg =
            ProcessTreeStep context
    in
        case context.source of
            AmazonS3 sourceData ->
                AmazonS3.makeTree msg sourceData context.treeMarker
