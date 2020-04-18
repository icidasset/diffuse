module UI.Authentication.ContextMenu exposing (moreOptionsMenu)

import ContextMenu exposing (..)
import Coordinates exposing (Coordinates)
import Svg
import UI.Authentication.Types as Authentication
import UI.Svg.Elements
import UI.Types exposing (Msg(..))



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
        , Item
            { icon = \_ _ -> Svg.map never UI.Svg.Elements.textileLogo
            , label = "Textile (Experimental)"
            , msg = AuthenticationMsg Authentication.PingTextile
            , active = False
            }
        ]
