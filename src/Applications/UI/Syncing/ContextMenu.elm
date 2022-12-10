module UI.Syncing.ContextMenu exposing (syncDataMenu)

import Chunky exposing (inline)
import ContextMenu exposing (..)
import Coordinates exposing (Coordinates)
import Html
import Svg
import UI.Kit
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
            { icon = \_ _ -> Svg.map never (UI.Svg.Elements.dropboxLogo 16)
            , label = "Dropbox"
            , msg = SyncingMsg <| Syncing.TriggerExternalAuth (Dropbox { accessToken = "", expiresAt = 0, refreshToken = "" }) ""
            , active = False
            }
        , Item
            { icon = \_ _ -> Svg.map never (UI.Svg.Elements.remoteStorageLogo 16)
            , label = "RemoteStorage"
            , msg =
                { icon = \size _ -> Svg.map never (UI.Svg.Elements.remoteStorageLogo size)
                , placeholder = "example@5apps.com"
                , question =
                    UI.Kit.askForInput
                        { question =
                            "What's your user address?"
                        , info =
                            [ Html.text "The format is "
                            , inline
                                []
                                [ Html.text "username@server.domain" ]
                            ]
                        }
                , value = ""
                }
                    |> Syncing.AskForInput
                        (RemoteStorage { userAddress = "", token = "" })
                    |> SyncingMsg
            , active = False
            }
        , Item
            { icon = \_ _ -> Svg.map never (UI.Svg.Elements.ipfsLogo 16)
            , label = "IPFS (using MFS)"
            , msg = SyncingMsg Syncing.PingIpfs
            , active = False
            }
        ]
