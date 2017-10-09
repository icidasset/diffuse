module Tracks.Collection.Responses exposing (..)

import Queue.Types
import Response.Ext exposing (do, doDelayed)
import Time
import Tracks.Encoding
import Tracks.Ports as Ports
import Tracks.Types exposing (..)
import Types as TopLevel


{-| Consequences when changing `collection.untouched`.
-}
globalConsequences : Collection -> Collection -> Model -> Cmd TopLevel.Msg
globalConsequences oldCollection newCollection model =
    case oldCollection.untouched /= newCollection.untouched of
        True ->
            let
                encodedTracks =
                    List.map Tracks.Encoding.encodeTrack newCollection.untouched

                search =
                    TopLevel.TracksMsg (Search model.searchTerm)
            in
                Cmd.batch
                    [ Ports.updateSearchIndex encodedTracks
                    , do TopLevel.AutoGeneratePlaylists
                    , do search

                    -- Store data
                    , if model.initialImportPerformed then
                        do TopLevel.DebounceStoreUserData
                      else
                        Cmd.none
                    ]

        False ->
            Cmd.none


{-| Consequences when changing `collection.harvested`.
-}
harvestingConsequences : Collection -> Collection -> Model -> Cmd TopLevel.Msg
harvestingConsequences oldCollection newCollection _ =
    case oldCollection.harvested /= newCollection.harvested of
        True ->
            do (TopLevel.QueueMsg Queue.Types.Reset)

        False ->
            Cmd.none
