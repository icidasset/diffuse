module UI.Syncing.ContextMenu exposing (syncDataMenu)

import ContextMenu exposing (..)
import Coordinates exposing (Coordinates)
import Svg
import UI.Svg.Elements
import UI.Syncing.Types as Syncing
import UI.Types exposing (Msg(..))
import User.Layer exposing (Method(..))



-- 🔱


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
            { icon = \_ _ -> Svg.map never (UI.Svg.Elements.ipfsLogo 16)
            , label = "IPFS (using MFS)"
            , msg = SyncingMsg Syncing.PingIpfs
            , active = False
            }
        ]
