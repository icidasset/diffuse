module UI.Sources.State exposing (..)

import Alien
import Browser.Navigation as Nav
import Common
import Conditional exposing (ifThenElse)
import Coordinates
import Dict
import Dict.Ext as Dict
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Json
import Json.Encode
import Monocle.Lens as Lens
import Notifications
import Return exposing (andThen, return)
import Sources exposing (..)
import Sources.Encoding as Sources
import Sources.Services as Services
import Sources.Services.Dropbox
import Sources.Services.Google
import Tracks.Collection
import UI.Common.State as Common
import UI.Page as Page
import UI.Ports as Ports
import UI.Sources.ContextMenu as Sources
import UI.Sources.Form as Form
import UI.Sources.Page as Sources
import UI.Sources.Query
import UI.Sources.Types exposing (..)
import UI.Tracks.State as Tracks
import UI.Types as UI exposing (Manager, Model)
import UI.User.State.Export as User



-- 🌳


formLens =
    { get = .sourceForm
    , set = \form m -> { m | sourceForm = form }
    }


formContextLens =
    Lens.compose
        formLens
        { get = .context
        , set = \context m -> { m | context = context }
        }


formStepLens =
    Lens.compose
        formLens
        { get = .step
        , set = \step m -> { m | step = step }
        }



-- 📣


update : Msg -> Manager
update msg =
    case msg of
        Bypass ->
            Return.singleton

        --
        FinishedProcessingSource a ->
            finishedProcessingSource a

        FinishedProcessing ->
            finishedProcessing

        Process ->
            process

        ProcessSpecific a ->
            processSpecific a

        ReportProcessingError a ->
            reportProcessingError a

        ReportProcessingProgress a ->
            reportProcessingProgress a

        StopProcessing ->
            stopProcessing

        -----------------------------------------
        -- Collection
        -----------------------------------------
        AddToCollection a ->
            addToCollection a

        RemoveFromCollection a ->
            removeFromCollection a

        UpdateSourceData a ->
            updateSourceData a

        -----------------------------------------
        -- Form
        -----------------------------------------
        AddSourceUsingForm ->
            addSourceUsingForm

        EditSourceUsingForm ->
            editSourceUsingForm

        RenameSourceUsingForm ->
            renameSourceUsingForm

        ReturnToIndex ->
            returnToIndex

        SelectService a ->
            selectService a

        SetFormData a b ->
            setFormData a b

        TakeStep ->
            takeStep

        TakeStepBackwards ->
            takeStepBackwards

        -----------------------------------------
        -- Individual
        -----------------------------------------
        SourceContextMenu a b ->
            sourceContextMenu a b

        ToggleActivation a ->
            toggleActivation a

        ToggleDirectoryPlaylists a ->
            toggleDirectoryPlaylists a

        ToggleProcessAutomatically ->
            toggleProcessAutomatically



-- 🔱


addSourcesFromUrl : Manager
addSourcesFromUrl model =
    case UI.Sources.Query.sourcesFromUrl model.url of
        [] ->
            Return.singleton model

        sources ->
            sources
                |> List.foldl
                    (\s -> andThen <| addToCollection s)
                    (Return.singleton model)
                |> Return.command
                    (Nav.replaceUrl
                        model.navKey
                        (model.url.path ++ Page.toString model.page)
                    )


finishedProcessing : Manager
finishedProcessing model =
    (case model.processingNotificationId of
        Just id ->
            Common.dismissNotification { id = id }

        Nothing ->
            Return.singleton
    )
        { model | processingContext = [] }


finishedProcessingSource : Json.Value -> Manager
finishedProcessingSource json model =
    case Json.decodeValue Json.string json of
        Ok sourceId ->
            model.processingContext
                |> List.filter (Tuple.first >> (/=) sourceId)
                |> (\newContext -> { model | processingContext = newContext })
                |> Return.singleton

        Err _ ->
            Return.singleton model


process : Manager
process model =
    case sourcesToProcess model of
        [] ->
            Return.singleton model

        toProcess ->
            if model.isOnline then
                processSpecific toProcess model

            else
                toProcess
                    |> List.filter Sources.worksOffline
                    |> (\s ->
                            case s of
                                [] ->
                                    Return.singleton model

                                _ ->
                                    processSpecific s model
                       )


processSpecific : List Source -> Manager
processSpecific toProcess model =
    let
        notification =
            Notifications.stickyCasual "Processing sources ..."

        notificationId =
            Notifications.id notification

        newNotifications =
            List.filter
                (\n -> Notifications.kind n /= Notifications.Error)
                model.notifications

        processingContext =
            toProcess
                |> List.sortBy (.data >> Dict.fetch "name" "")
                |> List.map (\{ id } -> ( id, 0 ))

        newModel =
            { model
                | notifications = newNotifications
                , processingContext = processingContext
                , processingError = Nothing
                , processingNotificationId = Just notificationId
            }
    in
    [ ( "origin"
      , Json.Encode.string (Common.urlOrigin model.url)
      )
    , ( "sources"
      , Json.Encode.list Sources.encode toProcess
      )
    ]
        |> Json.Encode.object
        |> Alien.broadcast Alien.ProcessSources
        |> Ports.toBrain
        |> return newModel
        |> andThen (Common.showNotification notification)


reportProcessingError : Json.Value -> Manager
reportProcessingError json model =
    case Json.decodeValue (Json.dict Json.string) json of
        Ok dict ->
            let
                args =
                    { error = Dict.fetch "error" "" dict
                    , sourceId = Dict.fetch "sourceId" "" dict
                    }
            in
            []
                |> Notifications.errorWithCode
                    ("Could not process the _"
                        ++ Dict.fetch "sourceName" "" dict
                        ++ "_ source. I got the following response from the source:"
                    )
                    (Dict.fetch "error" "missingError" dict)
                |> Common.showNotificationWithModel
                    { model | processingError = Just args }

        Err _ ->
            "Could not decode processing error"
                |> Notifications.stickyError
                |> Common.showNotificationWithModel model


reportProcessingProgress : Json.Value -> Manager
reportProcessingProgress json model =
    case
        Json.decodeValue
            (Json.map2
                (\p s ->
                    { progress = p
                    , sourceId = s
                    }
                )
                (Json.field "progress" Json.float)
                (Json.field "sourceId" Json.string)
            )
            json
    of
        Ok { progress, sourceId } ->
            model.processingContext
                |> List.map
                    (\( sid, pro ) ->
                        ifThenElse (sid == sourceId)
                            ( sid, progress )
                            ( sid, pro )
                    )
                |> (\processingContext ->
                        { model | processingContext = processingContext }
                   )
                |> Return.singleton

        Err _ ->
            "Could not decode processing progress"
                |> Notifications.stickyError
                |> Common.showNotificationWithModel model


stopProcessing : Manager
stopProcessing model =
    case model.processingNotificationId of
        Just notificationId ->
            Alien.StopProcessing
                |> Alien.trigger
                |> Ports.toBrain
                |> return
                    { model
                        | processingContext = []
                        , processingNotificationId = Nothing
                    }
                |> andThen (Common.dismissNotification { id = notificationId })

        Nothing ->
            Return.singleton model



-- COLLECTION


addToCollection : Source -> Manager
addToCollection unsuitableSource model =
    let
        source =
            setProperId
                (List.length model.sources + 1)
                model.currentTime
                unsuitableSource
    in
    { model | sources = model.sources ++ [ source ] }
        |> User.saveSources
        |> andThen (processSpecific [ source ])


removeFromCollection : { sourceId : String } -> Manager
removeFromCollection { sourceId } model =
    model.sources
        |> List.filter (.id >> (/=) sourceId)
        |> (\c -> { model | sources = c })
        |> Return.singleton
        |> andThen User.saveSources
        |> andThen (Tracks.removeBySourceId sourceId)


updateSourceData : Json.Value -> Manager
updateSourceData json model =
    json
        |> Sources.decode
        |> Maybe.map
            (\source ->
                List.map
                    (\s ->
                        if s.id == source.id then
                            source

                        else
                            s
                    )
                    model.sources
            )
        |> Maybe.map (\col -> { model | sources = col })
        |> Maybe.withDefault model
        |> User.saveSources



-- FORM


addSourceUsingForm : Manager
addSourceUsingForm model =
    let
        context =
            model.sourceForm.context

        cleanContext =
            { context | data = Dict.map (always String.trim) context.data }
    in
    model
        |> formLens.set Form.initialModel
        |> addToCollection cleanContext
        |> andThen returnToIndex


editSourceUsingForm : Manager
editSourceUsingForm model =
    model
        |> formLens.set Form.initialModel
        |> replaceSourceInCollection model.sourceForm.context
        |> andThen process
        |> andThen returnToIndex


renameSourceUsingForm : Manager
renameSourceUsingForm model =
    model
        |> formLens.set Form.initialModel
        |> replaceSourceInCollection model.sourceForm.context
        |> andThen returnToIndex


returnToIndex : Manager
returnToIndex =
    Common.changeUrlUsingPage (Page.Sources Sources.Index)


selectService : String -> Manager
selectService serviceKey model =
    case Services.keyToType serviceKey of
        Just service ->
            model
                |> Lens.modify
                    formContextLens
                    (\c ->
                        { c
                            | data = Services.initialData service
                            , service = service
                        }
                    )
                |> Return.singleton

        Nothing ->
            Return.singleton model


setFormData : String -> String -> Manager
setFormData key value model =
    model
        |> Lens.modify
            formContextLens
            (\context ->
                context.data
                    |> Dict.insert key value
                    |> (\data -> { context | data = data })
            )
        |> Return.singleton


takeStep : Manager
takeStep model =
    let
        form =
            formLens.get model
    in
    case ( form.step, form.context.service ) of
        ( How, Dropbox ) ->
            form.context.data
                |> Sources.Services.Dropbox.authorizationUrl
                |> externalAuthorization model

        ( How, Google ) ->
            form.context.data
                |> Sources.Services.Google.authorizationUrl
                |> externalAuthorization model

        _ ->
            model
                |> Lens.modify formStepLens takeStepForwards
                |> Return.singleton


takeStepBackwards : Manager
takeStepBackwards =
    Lens.modify formStepLens takeStepBackwards_ >> Return.singleton



-- INDIVIDUAL


sourceContextMenu : Source -> Mouse.Event -> Manager
sourceContextMenu source mouseEvent model =
    mouseEvent.clientPos
        |> Coordinates.fromTuple
        |> Sources.sourceMenu source
        |> Common.showContextMenuWithModel model


toggleActivation : { sourceId : String } -> Manager
toggleActivation { sourceId } model =
    model.sources
        |> List.map
            (\source ->
                if source.id == sourceId then
                    { source | enabled = not source.enabled }

                else
                    source
            )
        |> (\collection -> { model | sources = collection })
        |> Tracks.reviseCollection Tracks.Collection.identify
        |> andThen Common.forceTracksRerender
        |> andThen Common.generateDirectoryPlaylists
        |> andThen User.saveSources


toggleDirectoryPlaylists : { sourceId : String } -> Manager
toggleDirectoryPlaylists { sourceId } model =
    model.sources
        |> List.map
            (\source ->
                if source.id == sourceId then
                    { source | directoryPlaylists = not source.directoryPlaylists }

                else
                    source
            )
        |> (\collection -> { model | sources = collection })
        |> User.saveSources
        |> andThen Common.generateDirectoryPlaylists


toggleProcessAutomatically : Manager
toggleProcessAutomatically model =
    User.saveSettings { model | processAutomatically = not model.processAutomatically }



-- ⚗️


externalAuthorization : Model -> (String -> String) -> ( Model, Cmd UI.Msg )
externalAuthorization model urlBuilder =
    model.url
        |> Common.urlOrigin
        |> urlBuilder
        |> Nav.load
        |> return model


replaceSourceInCollection : Source -> Manager
replaceSourceInCollection source model =
    model.sources
        |> List.map (\s -> ifThenElse (s.id == source.id) source s)
        |> (\s -> { model | sources = s })
        |> User.saveSources


sourcesToProcess : Model -> List Source
sourcesToProcess model =
    List.filter (.enabled >> (==) True) model.sources


takeStepForwards : FormStep -> FormStep
takeStepForwards currentStep =
    case currentStep of
        Where ->
            How

        _ ->
            By


takeStepBackwards_ : FormStep -> FormStep
takeStepBackwards_ currentStep =
    case currentStep of
        By ->
            How

        _ ->
            Where
