port module Brain.Ports exposing (fromCache, fromUI, receiveTags, removeCache, requestCache, requestTags, toCache, toUI)

import Alien
import Sources.Processing exposing (ContextForTags)



-- 📣


port removeCache : Alien.Event -> Cmd msg


port requestCache : Alien.Event -> Cmd msg


port requestTags : ContextForTags -> Cmd msg


port toCache : Alien.Event -> Cmd msg


port toUI : Alien.Event -> Cmd msg



-- 📰


port fromCache : (Alien.Event -> msg) -> Sub msg


port fromUI : (Alien.Event -> msg) -> Sub msg


port receiveTags : (ContextForTags -> msg) -> Sub msg
