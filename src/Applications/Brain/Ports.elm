port module Brain.Ports exposing (fromAlien, receiveSearchResults, receiveTags, removeCache, removeIpfs, requestCache, requestIpfs, requestSearch, requestTags, toCache, toIpfs, toUI, updateSearchIndex)

import Alien
import Json.Encode as Json
import Sources.Processing exposing (ContextForTags)



-- 📣


port removeCache : Alien.Event -> Cmd msg


port removeIpfs : Alien.Event -> Cmd msg


port requestCache : Alien.Event -> Cmd msg


port requestIpfs : Alien.Event -> Cmd msg


port requestSearch : String -> Cmd msg


port requestTags : ContextForTags -> Cmd msg


port toCache : Alien.Event -> Cmd msg


port toIpfs : Alien.Event -> Cmd msg


port toUI : Alien.Event -> Cmd msg


port updateSearchIndex : Json.Value -> Cmd msg



-- 📰


port fromAlien : (Alien.Event -> msg) -> Sub msg


port receiveSearchResults : (List String -> msg) -> Sub msg


port receiveTags : (ContextForTags -> msg) -> Sub msg
