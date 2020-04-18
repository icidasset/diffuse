module UI.Authentication.Types exposing (Msg(..), Question, State(..))

import Html.Events.Extra.Mouse as Mouse
import Http
import Json.Decode as Json
import User.Layer exposing (Method(..))
import User.Layer.Methods.RemoteStorage as RemoteStorage



-- 🌳


type State
    = Authenticated Method
    | InputScreen Method Question
    | NewEncryptionKeyScreen Method (Maybe String)
    | UpdateEncryptionKeyScreen Method (Maybe String)
    | Unauthenticated
    | Welcome


type alias Question =
    { placeholder : String
    , question : String
    , value : String
    }



-- 📣


type Msg
    = Bypass
      --
    | BootFailure String
    | CancelFlow
    | GetStarted
    | NotAuthenticated
    | RemoteStorageWebfinger RemoteStorage.Attributes (Result Http.Error String)
    | ShowMoreOptions Mouse.Event
    | SignIn Method
    | SignInWithPassphrase Method String
    | SignedIn Json.Value
    | SignOut
    | TriggerExternalAuth Method String
      -----------------------------------------
      -- Encryption
      -----------------------------------------
    | KeepPassphraseInMemory String
    | MissingSecretKey Json.Value
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
      -----------------------------------------
      -- Textile
      -----------------------------------------
    | PingTextile
    | PingTextileCallback (Result Http.Error ())
    | PingOtherTextile String
    | PingOtherTextileCallback String (Result Http.Error ())
