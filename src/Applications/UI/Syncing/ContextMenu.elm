module UI.Syncing.ContextMenu exposing (moreOptionsMenu, syncDataMenu)

import ContextMenu exposing (..)
import Coordinates exposing (Coordinates)
import Svg
import UI.Svg.Elements
import UI.Syncing.Types as Syncing
import UI.Types exposing (Msg(..))
import User.Layer exposing (Method(..))



-- 🔱


moreOptionsMenu : Coordinates -> ContextMenu Msg
moreOptionsMenu =
    ContextMenu
        [ Item
            { icon = \_ _ -> Svg.map never UI.Svg.Elements.ipfsLogo
            , label = "IPFS (using the Mutable File System)"
            , msg = SyncingMsg Syncing.PingIpfs
            , active = False
            }
        ]


syncDataMenu : Coordinates -> ContextMenu Msg
syncDataMenu =
    ContextMenu
        [ Item
            { icon = \_ _ -> Svg.map never UI.Svg.Elements.fissionLogo
            , label = "Fission"
            , msg = SyncingMsg <| Syncing.TriggerExternalAuth (Fission { initialised = False }) ""
            , active = False
            }
        , Item
            { icon = \_ _ -> Svg.map never UI.Svg.Elements.dropboxLogo
            , label = "Dropbox"
            , msg = SyncingMsg <| Syncing.TriggerExternalAuth (Dropbox { accessToken = "", expiresAt = 0, refreshToken = "" }) ""
            , active = False
            }
        , Item
            { icon = \_ _ -> Svg.map never UI.Svg.Elements.remoteStorageLogo
            , label = "RemoteStorage"
            , msg = Bypass -- TODO
            , active = False
            }
        , Item
            { icon = \_ _ -> Svg.map never UI.Svg.Elements.ipfsLogo
            , label = "IPFS (using MFS)"
            , msg = SyncingMsg Syncing.PingIpfs
            , active = False
            }
        ]
