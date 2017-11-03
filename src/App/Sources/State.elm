module Sources.State exposing (..)

import Dict
import Http exposing (Error(..))
import List.Extra as List
import Maybe.Ext as Maybe
import Maybe.Extra as Maybe
import Navigation
import Response.Ext exposing (do)
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
    , form = NewForm 1 initialSource
    }


initialSource : Source
initialSource =
    makeSource AmazonS3 (Services.initialData AmazonS3)



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
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
                    , sourcesHaveUpdated newCollection
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
                    , sourcesHaveUpdated newCollection
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
                    [ sourcesHaveUpdated newCollection ]


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
    Sub.none
