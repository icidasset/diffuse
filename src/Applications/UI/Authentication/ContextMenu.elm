module UI.Authentication.ContextMenu exposing (moreOptionsMenu, syncDataMenu)

import ContextMenu exposing (..)
import Coordinates exposing (Coordinates)
import Svg
import UI.Authentication.Types as Authentication
import UI.Svg.Elements
import UI.Types exposing (Msg(..))
import User.Layer exposing (Method(..))



-- 🔱


moreOptionsMenu : Coordinates -> ContextMenu Msg
moreOptionsMenu =
    ContextMenu
        [ Item
            { icon = \_ _ -> Svg.map never UI.Svg.Elements.ipfsLogo
            , label = "IPFS (using the Mutable File System)"
            , msg = AuthenticationMsg Authentication.PingIpfs
            , active = False
            }
        ]


syncDataMenu : Coordinates -> ContextMenu Msg
syncDataMenu =
    ContextMenu
        [ Item
            { icon = \_ _ -> Svg.map never UI.Svg.Elements.fissionLogo
            , label = "Fission"
            , msg = AuthenticationMsg <| Authentication.TriggerExternalAuth (Fission { initialised = False }) ""
            , active = False
            }
        , Item
            { icon = \_ _ -> Svg.map never UI.Svg.Elements.dropboxLogo
            , label = "Dropbox"
            , msg = AuthenticationMsg <| Authentication.TriggerExternalAuth (Dropbox { accessToken = "", expiresAt = 0, refreshToken = "" }) ""
            , active = False
            }
        , Item
            { icon = \_ _ -> Svg.map never UI.Svg.Elements.remoteStorageLogo
            , label = "RemoteStorage"
            , msg = Bypass
            , active = False
            }
        , Item
            { icon = \_ _ -> Svg.map never UI.Svg.Elements.ipfsLogo
            , label = "IPFS (using MFS)"
            , msg = AuthenticationMsg Authentication.PingIpfs
            , active = False
            }
        ]
