module Tracks.State exposing (..)

import List.Extra as List
import Tracks.Types exposing (..)
import Tracks.Utils exposing (..)
import Types as TopLevel
import Utils exposing (do)


-- 💧


initialModel : TopLevel.ProgramFlags -> Model
initialModel flags =
    { collection = decodeTracks flags
    }


initialCommands : Cmd TopLevel.Msg
initialCommands =
    Cmd.none



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        -- # Add
        -- > Add tracks to the collection.
        --
        Add additionalTracks ->
            let
                col =
                    additionalTracks
                        |> List.append model.collection
                        |> List.sortBy trackSortComparable
            in
                ($)
                    { model | collection = col }
                    []
                    [ do TopLevel.FillQueue, storeTracks col ]

        -- # Remove
        -- > Remove tracks from the collection,
        --   matching by the `sourceId`.
        --
        Remove sourceId ->
            let
                col =
                    List.filter
                        (\t -> t.sourceId /= sourceId)
                        model.collection
            in
                ($)
                    { model | collection = col }
                    []
                    [ do TopLevel.FillQueue, storeTracks col ]

        -- # Remove
        -- > Remove tracks from the collection,
        --   matching by the `sourceId` and the `path`.
        --
        RemoveByPath sourceId pathsList ->
            let
                col =
                    List.filter
                        (\t ->
                            if t.sourceId == sourceId then
                                List.notMember t.path pathsList
                            else
                                True
                        )
                        model.collection
            in
                ($)
                    { model | collection = col }
                    []
                    [ do TopLevel.FillQueue, storeTracks col ]



-- Utils


{-| Sort.
-}
trackSortComparable : Track -> String
trackSortComparable t =
    t.tags.title
        |> String.append (toString t.tags.nr)
        |> String.append t.tags.album
        |> String.append t.tags.artist
