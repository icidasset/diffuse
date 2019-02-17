port module Brain.Ports exposing (fromCache, fromUI, receiveSearchResults, receiveTags, removeCache, requestCache, requestSearch, requestTags, toCache, toUI, updateSearchIndex)

import Alien
import Json.Encode as Json
import Sources.Processing exposing (ContextForTags)



-- 📣


port removeCache : Alien.Event -> Cmd msg


port requestCache : Alien.Event -> Cmd msg


port requestSearch : String -> Cmd msg


port requestTags : ContextForTags -> Cmd msg


port toCache : Alien.Event -> Cmd msg


port toUI : Alien.Event -> Cmd msg


port updateSearchIndex : Json.Value -> Cmd msg



-- 📰


port fromCache : (Alien.Event -> msg) -> Sub msg


port fromUI : (Alien.Event -> msg) -> Sub msg


port receiveSearchResults : (List String -> msg) -> Sub msg


port receiveTags : (ContextForTags -> msg) -> Sub msg
