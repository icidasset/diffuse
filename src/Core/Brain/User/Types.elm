module Brain.User.Types exposing (..)

import Debouncer.Basic as Debouncer
import Json.Decode as Json
import Url exposing (Url)
import User.Layer as User exposing (HypaethralBit, HypaethralData)
import User.Layer.Methods.Dropbox as Dropbox



-- 📣


type Msg
    = Commence (Maybe User.Method) Url ( Json.Value, HypaethralData )
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
    | FinishedSyncing
    | GotHypaethralData HypaethralData
    | SaveHypaethralDataBits (List HypaethralBit)
    | SaveHypaethralDataSlowly (Debouncer.Msg HypaethralBit)
      -----------------------------------------
      -- Encryption
      -----------------------------------------
    | RemoveEncryptionKey
    | UpdateEncryptionKey Json.Value
      -----------------------------------------
      -- 📭 Other
      -----------------------------------------
    | RefreshedDropboxTokens { currentTime : Int, refreshToken : String } Dropbox.Tokens Msg
