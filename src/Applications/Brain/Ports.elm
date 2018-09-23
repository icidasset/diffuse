port module Brain.Ports exposing (fromCache, fromUI, removeCache, requestCache, toCache, toUI)

import Alien



-- 📣


port removeCache : Alien.Event -> Cmd msg


port requestCache : Alien.Event -> Cmd msg


port toCache : Alien.Event -> Cmd msg


port toUI : Alien.Event -> Cmd msg



-- 📰


port fromCache : (Alien.Event -> msg) -> Sub msg


port fromUI : (Alien.Event -> msg) -> Sub msg
