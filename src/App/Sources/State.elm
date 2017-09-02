module Sources.State exposing (..)

import Date
import Dict
import Http exposing (Error(..))
import List.Extra as List
import Maybe.Ext as Maybe
import Maybe.Extra as Maybe
import Navigation
import Response.Ext exposing (do)
import Sources.Ports as Ports
import Sources.Processing as Processing
import Sources.Services as Services exposing (makeSource)
import Sources.Types exposing (..)
import Sources.Utils exposing (..)
import Tracks.Types exposing (emptyTrack)
import Types as TopLevel


-- 💧


initialModel : Model
initialModel =
    { collection = []
    , isProcessing = Nothing
    , form = NewForm 1 initialSource
    , processingErrors = []
    , timestamp = Date.fromTime 0
    }


initialSource : Source
initialSource =
    makeSource AmazonS3 (Services.initialData AmazonS3)



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        ------------------------------------
        -- Process
        ------------------------------------
        {- If already processing, do nothing.
           If there are no sources, do nothing.
           If there are sources, start processing the first source.
        -}
        Process tracks ->
            let
                processingData =
                    List.map
                        (\source ->
                            ( source
                            , List.filter (\t -> t.sourceId == source.id) tracks
                            )
                        )
                        model.collection

                isProcessing =
                    model.collection
                        |> List.head
                        |> Maybe.map (always processingData)
                        |> Maybe.preferFirst model.isProcessing

                command =
                    model.collection
                        |> List.head
                        |> Maybe.map (Processing.takeFirstStep model.timestamp)
                        |> Maybe.preferSecond (Maybe.map (always Cmd.none) model.isProcessing)
                        |> Maybe.withDefault Cmd.none

                processingErrors =
                    model.isProcessing
                        |> Maybe.map (\_ -> model.processingErrors)
                        |> Maybe.withDefault []
            in
                ($)
                    { model | isProcessing = isProcessing, processingErrors = processingErrors }
                    [ command ]
                    []

        {- If not processing, do nothing.
           If there are no sources left, do nothing.
           If there are sources left, start processing the next source in line.
        -}
        ProcessNextInLine ->
            let
                takeStep =
                    Processing.takeFirstStep model.timestamp

                maybe =
                    model.isProcessing
                        |> Maybe.andThen (List.tail)
                        |> Maybe.andThen (\a -> Maybe.map ((,) a) (List.head a))
                        |> Maybe.map (Tuple.mapSecond Tuple.first)
                        |> Maybe.map (Tuple.mapSecond takeStep)
            in
                ($)
                    { model | isProcessing = Maybe.map Tuple.first maybe }
                    [ maybe
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault Cmd.none
                    ]
                    []

        {- Processing step,
           Phase 1, `makeTree`.
           ie. make a file list/tree.
        -}
        ProcessTreeStep ctx (Ok resp) ->
            let
                associatedTracks =
                    model.isProcessing
                        |> Maybe.andThen List.head
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault []
            in
                ($)
                    model
                    [ Processing.takeTreeStep ctx resp associatedTracks model.timestamp ]
                    []

        --
        -- Error
        --
        ProcessTreeStep ctx (Err err) ->
            let
                publicError =
                    case err of
                        NetworkError ->
                            "Cannot connect to this source"

                        Timeout ->
                            "Source did not respond (timeout)"

                        BadStatus response ->
                            Services.parseErrorResponse ctx.source.service response.body

                        _ ->
                            toString err
            in
                ($)
                    { model
                        | processingErrors =
                            ( ctx.source.id, publicError ) :: model.processingErrors
                    }
                    [ do ProcessNextInLine ]
                    []

        --
        -- Remove tracks
        --
        ProcessTreeStepRemoveTracks sourceId filePaths ->
            ($)
                model
                []
                [ filePaths
                    |> Tracks.Types.RemoveByPath sourceId
                    |> TopLevel.TracksMsg
                    |> do
                ]

        {- Processing step,
           Phase 2, `makeTags`.
           ie. get the tags for each file in the file list.
        -}
        ProcessTagsStep tagsCtx ->
            let
                insert =
                    tagsCtx
                        |> Processing.tracksFromTagsContext
                        |> Tracks.Types.Add
                        |> TopLevel.TracksMsg
                        |> do

                cmd =
                    model.isProcessing
                        |> Maybe.map (List.map Tuple.first)
                        |> Maybe.andThen (Processing.findTagsContextSource tagsCtx)
                        |> Maybe.andThen (Processing.takeTagsStep model.timestamp tagsCtx)
                        |> Maybe.withDefault (do ProcessNextInLine)
            in
                ($) model [ cmd ] [ insert ]

        ------------------------------------
        -- CRUD
        ------------------------------------
        Destroy sourceId ->
            let
                newCollection =
                    List.filter (\s -> s.id /= sourceId) model.collection
            in
                ($)
                    { model | collection = newCollection }
                    []
                    [ sourceId
                        |> Tracks.Types.Remove
                        |> TopLevel.TracksMsg
                        |> do
                    , updateEnabledSourceIds newCollection
                    , do TopLevel.StoreUserData
                    ]

        ------------------------------------
        -- Forms
        ------------------------------------
        --
        -- Assign
        --
        AssignFormProperty key value ->
            let
                updateSource =
                    \source ->
                        { source | data = Dict.insert key value source.data }

                updatedForm =
                    case model.form of
                        NewForm step source ->
                            NewForm step (updateSource source)

                        EditForm source ->
                            EditForm (updateSource source)
            in
                (!) { model | form = updatedForm } []

        --
        AssignFormService serviceKey ->
            let
                service =
                    Services.keyToType serviceKey

                newSource =
                    makeSource service (Services.initialData service)

                updatedForm =
                    case model.form of
                        NewForm step _ ->
                            NewForm step newSource

                        EditForm _ ->
                            EditForm newSource
            in
                (!) { model | form = updatedForm } []

        --
        AssignFormStep newStep ->
            let
                updatedForm =
                    case model.form of
                        NewForm _ source ->
                            NewForm newStep source

                        EditForm source ->
                            EditForm source
            in
                (!) { model | form = updatedForm } []

        --
        -- Submit
        --
        SubmitForm ->
            let
                ns =
                    case model.form of
                        NewForm _ source ->
                            source

                        EditForm source ->
                            source

                source =
                    { ns | data = Dict.map (always String.trim) ns.data }

                newCollection =
                    case model.form of
                        NewForm _ _ ->
                            (setProperSourceId model source) :: model.collection

                        EditForm _ ->
                            model.collection
                                |> List.filter (.id >> (/=) source.id)
                                |> List.append [ source ]
            in
                ($)
                    { model
                        | collection = newCollection
                        , form = NewForm 1 initialSource
                    }
                    []
                    [ do TopLevel.ProcessSources
                    , Navigation.newUrl
                        (case List.length model.collection of
                            0 ->
                                "/"

                            _ ->
                                "/sources"
                        )
                    , updateEnabledSourceIds newCollection
                    , do TopLevel.StoreUserData
                    ]

        ------------------------------------
        -- Other
        ------------------------------------
        ToggleSource source ->
            let
                newCollection =
                    List.updateIf
                        (.id >> (==) source.id)
                        (\s -> { s | enabled = not s.enabled })
                        model.collection
            in
                (!)
                    { model | collection = newCollection }
                    [ updateEnabledSourceIds newCollection
                    , do TopLevel.StoreUserData
                    ]


editForm : Model -> SourceId -> Model
editForm model sourceId =
    let
        source =
            model.collection
                |> List.find (.id >> (==) sourceId)
                |> Maybe.withDefault initialSource
    in
        { model | form = EditForm source }


newForm : Model -> Model
newForm model =
    { model | form = NewForm 1 initialSource }



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.receiveTags ProcessTagsStep ]
