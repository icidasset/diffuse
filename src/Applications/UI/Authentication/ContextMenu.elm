module UI.Authentication.ContextMenu exposing (moreOptionsMenu)

import Authentication
import ContextMenu exposing (..)
import Coordinates exposing (Coordinates)
import Svg
import UI.Authentication as Authentication
import UI.Core exposing (Msg(..))
import UI.Svg.Elements



-- 🔱


moreOptionsMenu : Coordinates -> ContextMenu Msg
moreOptionsMenu =
    ContextMenu
        [ Item
            { icon = \_ _ -> Svg.map never UI.Svg.Elements.ipfsLogo
            , label = "IPFS"
            , msg = AuthenticationMsg Authentication.PingIpfs
            , active = False
            }
        ]
