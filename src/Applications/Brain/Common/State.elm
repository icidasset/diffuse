module Brain.Common.State exposing (..)

import Alien
import Brain.Ports as Ports
import Brain.Types exposing (..)
import Json.Decode as Json
import Return
import Return.Ext as Return



-- 🔱


giveUI : Alien.Tag -> Json.Value -> Manager
giveUI tag data =
    data
        |> Alien.broadcast tag
        |> Ports.toUI
        |> Return.communicate


nudgeUI : Alien.Tag -> Manager
nudgeUI tag =
    tag
        |> Alien.trigger
        |> Ports.toUI
        |> Return.communicate


reportUI : Alien.Tag -> String -> Manager
reportUI tag error =
    error
        |> Alien.report tag
        |> Ports.toUI
        |> Return.communicate
