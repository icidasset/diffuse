module Themes.Sunrise.Settings.Common exposing (..)

import Material.Icons.Round as Icons
import UI.Navigation exposing (..)
import UI.Syncing.Types as Syncing
import UI.Types



-- SYNCING


changePassphrase method =
    ( Icon Icons.lock
    , Label "Change Passphrase" Shown
    , method
        |> Syncing.ShowUpdateEncryptionKeyScreen
        |> UI.Types.SyncingMsg
        |> PerformMsg
    )
