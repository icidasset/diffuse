port module Brain.Ports exposing (..)

import Alien
import Json.Encode as Json
import Sources.Processing exposing (ContextForTags, ContextForTagsSync)



-- 📣


port downloadTracks : Json.Value -> Cmd msg


port fabricateSecretKey : Alien.Event -> Cmd msg


port removeCache : Alien.Event -> Cmd msg


port removeTracksFromCache : Json.Value -> Cmd msg


port requestCache : Alien.Event -> Cmd msg


port requestSearch : String -> Cmd msg


port requestTags : ContextForTags -> Cmd msg


port storeTracksInCache : Json.Value -> Cmd msg


port syncTags : ContextForTagsSync -> Cmd msg


port toCache : Alien.Event -> Cmd msg


port toUI : Alien.Event -> Cmd msg


port updateSearchIndex : Json.Value -> Cmd msg



-- 📣  ░░  USER LAYER SERVICES


port deconstructFission : () -> Cmd msg


port deconstructRemoteStorage : () -> Cmd msg


port handlePendingFissionAuthorisation : () -> Cmd msg


port provideArtworkTrackUrls : Json.Value -> Cmd msg


port requestDropbox : Alien.Event -> Cmd msg


port requestFission : Alien.Event -> Cmd msg


port requestIpfs : Alien.Event -> Cmd msg


port requestLegacyLocalData : Alien.Event -> Cmd msg


port requestRemoteStorage : Alien.Event -> Cmd msg


port toDropbox : Alien.Event -> Cmd msg


port toFission : Alien.Event -> Cmd msg


port toIpfs : Alien.Event -> Cmd msg


port toRemoteStorage : Alien.Event -> Cmd msg



-- 📰


port fromAlien : (Alien.Event -> msg) -> Sub msg


port makeArtworkTrackUrls : (Json.Value -> msg) -> Sub msg


port receiveSearchResults : (List String -> msg) -> Sub msg


port receiveTags : (ContextForTags -> msg) -> Sub msg


port replaceTags : (ContextForTagsSync -> msg) -> Sub msg


port savedHypaethralBit : (Json.Value -> msg) -> Sub msg
