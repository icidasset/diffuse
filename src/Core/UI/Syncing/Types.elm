module UI.Syncing.Types exposing (Msg(..), Question, State(..))

import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import Http
import Json.Decode as Json
import Material.Icons.Types exposing (Coloring)
import Svg exposing (Svg)
import User.Layer exposing (Method)
import User.Layer.Methods.Dropbox as Dropbox
import User.Layer.Methods.RemoteStorage as RemoteStorage



-- 🌳


type State
    = NotSynced
    | Synced Method
    | Syncing { method : Method, notificationId : Int }
    | InputScreen Method Question
    | NewEncryptionKeyScreen Method (Maybe String)
    | UpdateEncryptionKeyScreen Method (Maybe String)


type alias Question =
    { icon : Int -> Coloring -> Svg Msg
    , placeholder : String
    , question : { question : String, info : List (Html Msg) }
    , value : String
    }



-- 📣


type Msg
    = Bypass
      --
    | ActivateSync Method
    | ActivateSyncWithPassphrase Method String
    | BootFailure String
    | ExchangeDropboxAuthCode (Result Http.Error Dropbox.Tokens)
    | GotSyncMethod Json.Value
    | RemoteStorageWebfinger RemoteStorage.Attributes (Result Http.Error String)
    | ShowSyncDataMenu Mouse.Event
    | StartedSyncing Json.Value
    | StopSync
    | TriggerExternalAuth Method String
      -----------------------------------------
      -- Encryption
      -----------------------------------------
    | KeepPassphraseInMemory String
    | NeedEncryptionKey { error : String }
    | RemoveEncryptionKey Method
    | ShowNewEncryptionKeyScreen Method
    | ShowUpdateEncryptionKeyScreen Method
    | UpdateEncryptionKey Method String
      -----------------------------------------
      -- IPFS
      -----------------------------------------
    | PingIpfs
    | PingIpfsCallback (Result Http.Error ())
    | PingOtherIpfs String
    | PingOtherIpfsCallback String (Result Http.Error ())
      -----------------------------------------
      -- More Input
      -----------------------------------------
    | AskForInput Method Question
    | CancelInput
    | ConfirmInput
    | Input String
