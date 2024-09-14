module UI.Syncing.ContextMenu exposing (syncDataMenu)

import ContextMenu exposing (..)
import Coordinates exposing (Coordinates)
import Svg
import UI.Svg.Elements
import UI.Syncing.Common exposing (startDropbox, startIpfs, startRemoteStorage)
import UI.Types exposing (Msg)
import User.Layer exposing (dropboxMethod, ipfsMethod, methodName, remoteStorageMethod)



-- 🔱


syncDataMenu : Coordinates -> ContextMenu Msg
syncDataMenu =
    ContextMenu
        [ Item
            { icon = \_ _ -> Svg.map never (UI.Svg.Elements.dropboxLogo 16)
            , label = methodName dropboxMethod
            , msg = startDropbox
            , active = False
            }
        , Item
            { icon = \_ _ -> Svg.map never (UI.Svg.Elements.remoteStorageLogo 16)
            , label = methodName remoteStorageMethod
            , msg = startRemoteStorage
            , active = False
            }
        , Item
            { icon = \_ _ -> Svg.map never (UI.Svg.Elements.ipfsLogo 16)
            , label = methodName ipfsMethod
            , msg = startIpfs
            , active = False
            }
        ]
