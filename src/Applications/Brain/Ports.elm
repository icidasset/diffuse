port module Brain.Ports exposing (fabricateSecretKey, fromAlien, receiveSearchResults, receiveTags, removeCache, removeIpfs, requestCache, requestIpfs, requestRemoteStorage, requestSearch, requestTags, toCache, toIpfs, toRemoteStorage, toUI, updateSearchIndex)

import Alien
import Json.Encode as Json
import Sources.Processing exposing (ContextForTags)



-- 📣


port fabricateSecretKey : Alien.Event -> Cmd msg


port removeCache : Alien.Event -> Cmd msg


port removeIpfs : Alien.Event -> Cmd msg


port requestCache : Alien.Event -> Cmd msg


port requestIpfs : Alien.Event -> Cmd msg


port requestRemoteStorage : Alien.Event -> Cmd msg


port requestSearch : String -> Cmd msg


port requestTags : ContextForTags -> Cmd msg


port toCache : Alien.Event -> Cmd msg


port toRemoteStorage : Alien.Event -> Cmd msg


port toIpfs : Alien.Event -> Cmd msg


port toUI : Alien.Event -> Cmd msg


port updateSearchIndex : Json.Value -> Cmd msg



-- 📰


port fromAlien : (Alien.Event -> msg) -> Sub msg


port receiveSearchResults : (List String -> msg) -> Sub msg


port receiveTags : (ContextForTags -> msg) -> Sub msg
