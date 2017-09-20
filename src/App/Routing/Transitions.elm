module Routing.Transitions exposing (..)

import Response.Ext exposing (do)
import Routing.Types exposing (..)
import Sources.State as Sources
import Sources.Types
import Tracks.Types
import Types as TopLevel exposing (Msg(..))


type alias Response =
    ( TopLevel.Model, Cmd TopLevel.Msg )



-- 🍯


transition : Routing.Types.Msg -> TopLevel.Model -> Response -> Response
transition routingMsg oldModel response =
    let
        modelMapFn =
            case routingMsg of
                SetPage page ->
                    mapModel page

                _ ->
                    identity
    in
        response
            |> Tuple.mapFirst modelMapFn
            |> Tuple.mapSecond (mapCmd oldModel.routing.currentPage)



-- Commands


{-| Map the command based on the current page.
-}
mapCmd : Page -> Cmd TopLevel.Msg -> Cmd TopLevel.Msg
mapCmd currentPage cmd =
    case currentPage of
        Index ->
            Cmd.batch
                [ cmd
                , Tracks.Types.Recalibrate
                    |> TracksMsg
                    |> do
                ]

        _ ->
            cmd



-- Models


{-| Map the (top-level) model based on the next page.
-}
mapModel : Page -> TopLevel.Model -> TopLevel.Model
mapModel nextPage model =
    case nextPage of
        --
        -- When we are going to edit a source,
        -- set the `form` attribute.
        --
        Sources (Sources.Types.Edit sourceId) ->
            let
                sources =
                    Sources.editForm model.sources sourceId
            in
                { model | sources = sources }

        --
        -- When we are going to create a source,
        -- set the `form` attribute.
        --
        Sources Sources.Types.New ->
            case model.sources.form of
                Sources.Types.EditForm _ ->
                    { model | sources = Sources.newForm model.sources }

                _ ->
                    model

        --
        -- Default
        --
        _ ->
            model
