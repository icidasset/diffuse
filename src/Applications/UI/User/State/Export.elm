module UI.User.State.Export exposing (..)

import Alien
import Return
import UI.Ports as Ports
import UI.Types as UI exposing (..)
import User.Layer exposing (..)



-- 🔱


saveEnclosedUserData : Manager
saveEnclosedUserData model =
    { cachedTracks = model.cachedTracks
    , equalizerSettings = model.eqSettings
    , grouping = model.grouping
    , onlyShowCachedTracks = model.cachedTracksOnly
    , onlyShowFavourites = model.favouritesOnly
    , repeat = model.repeat
    , searchTerm = model.searchTerm
    , selectedPlaylist = Maybe.map .name model.selectedPlaylist
    , shuffle = model.shuffle
    , sortBy = model.sortBy
    , sortDirection = model.sortDirection
    }
        |> encodeEnclosedData
        |> Alien.broadcast Alien.SaveEnclosedUserData
        |> Ports.toBrain
        |> Return.return model
