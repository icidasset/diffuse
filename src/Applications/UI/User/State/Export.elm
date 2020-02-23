module UI.User.State.Export exposing (..)

import Alien
import Return
import UI.Ports as Ports
import UI.Types as UI exposing (..)
import User.Layer exposing (..)



-- 🔱


saveEnclosedUserData : Manager
saveEnclosedUserData model =
    let
        equalizerSettings =
            { low = model.equalizer.low
            , mid = model.equalizer.mid
            , high = model.equalizer.high
            , volume = model.equalizer.volume
            }
    in
    { cachedTracks = model.tracks.cached
    , equalizerSettings = equalizerSettings
    , grouping = model.tracks.grouping
    , onlyShowCachedTracks = model.tracks.cachedOnly
    , onlyShowFavourites = model.tracks.favouritesOnly
    , repeat = model.queue.repeat
    , searchTerm = model.tracks.searchTerm
    , selectedPlaylist = Maybe.map .name model.tracks.selectedPlaylist
    , shuffle = model.queue.shuffle
    , sortBy = model.tracks.sortBy
    , sortDirection = model.tracks.sortDirection
    }
        |> encodeEnclosedData
        |> Alien.broadcast Alien.SaveEnclosedUserData
        |> Ports.toBrain
        |> Return.return model
