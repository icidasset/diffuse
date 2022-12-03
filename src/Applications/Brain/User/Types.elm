module Brain.User.Types exposing (..)

import Debouncer.Basic as Debouncer
import Json.Decode as Json
import User.Layer as User exposing (HypaethralBit, HypaethralData)
import User.Layer.Methods.Dropbox as Dropbox
import Webnative



-- 📣


type Msg
    = Commence (Maybe User.Method) ( Json.Value, HypaethralData )
      -----------------------------------------
      --  Method
      -----------------------------------------
    | SetSyncMethod Json.Value
    | Sync
    | UnsetSyncMethod
      -----------------------------------------
      -- Enclosed Data
      -----------------------------------------
    | RetrieveEnclosedData
    | EnclosedDataRetrieved Json.Value
    | SaveEnclosedData Json.Value
      -----------------------------------------
      -- Hypaethral Data, pt. 1
      -----------------------------------------
    | SaveFavourites Json.Value
    | SavePlaylists Json.Value
    | SaveProgress Json.Value
    | SaveSettings Json.Value
    | SaveSources Json.Value
    | SaveTracks Json.Value
      -----------------------------------------
      -- Hypaethral Data, pt. 2
      -----------------------------------------
    | GotHypaethralData HypaethralData
    | GotWebnativeResponse Webnative.Response
    | SaveHypaethralDataBits (List HypaethralBit)
    | SaveHypaethralDataSlowly (Debouncer.Msg HypaethralBit)
      -----------------------------------------
      -- Encryption
      -----------------------------------------
    | FabricateSecretKey String
    | RemoveEncryptionKey
    | SecretKeyFabricated
    | UpdateEncryptionKey Json.Value
      -----------------------------------------
      -- 📭 Other
      -----------------------------------------
    | RefreshedDropboxTokens { currentTime : Int, refreshToken : String } Dropbox.Tokens Msg
