module UI.Syncing.Types exposing (Msg(..), Question, State(..))

import Html.Events.Extra.Mouse as Mouse
import Http
import Json.Decode as Json
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
    { placeholder : String
    , question : String
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
    | Input String
    | ConfirmInput
