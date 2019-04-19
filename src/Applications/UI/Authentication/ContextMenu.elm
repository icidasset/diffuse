module UI.Authentication.ContextMenu exposing (moreOptionsMenu)

import Authentication
import ContextMenu exposing (..)
import Coordinates exposing (Coordinates)
import Material.Icons.Action as Icons
import Svg
import UI.Authentication as Authentication
import UI.Core exposing (Msg(..))
import UI.Svg.Elements



-- 🔱


moreOptionsMenu : Coordinates -> ContextMenu Msg
moreOptionsMenu =
    ContextMenu
        [ ( \_ _ -> Svg.map never UI.Svg.Elements.ipfsLogo
          , "IPFS"
          , AuthenticationMsg (Authentication.ShowNewEncryptionKeyScreen Authentication.Ipfs)
          )

        -- , ( \_ _ -> Svg.map never UI.Svg.Elements.solidLogo
        --   , "Solid"
        --   , AuthenticationMsg (Authentication.ShowNewEncryptionKeyScreen Authentication.Ipfs)
        --   )
        ]
