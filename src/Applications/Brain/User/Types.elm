module Brain.User.Types exposing (..)

import Debouncer.Basic as Debouncer
import Json.Decode as Json
import User.Layer exposing (HypaethralBit)
import User.Layer.Methods.Dropbox as Dropbox
import Webnative



-- 📣


type Msg
    = SignIn Json.Value
    | SignOut
      -----------------------------------------
      -- 0. Secret Key
      -----------------------------------------
    | FabricateSecretKey String
    | SecretKeyFabricated
      -----------------------------------------
      -- 1. Method
      -----------------------------------------
    | RetrieveMethod
    | MethodRetrieved Json.Value
      -----------------------------------------
      -- 2. Data
      -----------------------------------------
    | RetrieveHypaethralData User.Layer.Method HypaethralBit
    | HypaethralDataRetrieved Json.Value
      -----------------------------------------
      -- x. Data
      -----------------------------------------
    | RetrieveEnclosedData
    | EnclosedDataRetrieved Json.Value
    | SaveEnclosedData Json.Value
      -----------------------------------------
      -- y. Data
      -----------------------------------------
    | SaveFavourites Json.Value
    | SavePlaylists Json.Value
    | SaveProgress Json.Value
    | SaveSettings Json.Value
    | SaveSources Json.Value
    | SaveTracks Json.Value
      -----------------------------------------
      -- z. Data
      -----------------------------------------
    | GotWebnativeResponse Webnative.Response
    | SaveAllHypaethralData
    | SaveHypaethralDataBit HypaethralBit
    | SaveHypaethralDataBits (List HypaethralBit)
    | SaveHypaethralDataSlowly (Debouncer.Msg HypaethralBit)
    | SaveNextHypaethralBit
      -----------------------------------------
      -- z. Secret Key
      -----------------------------------------
    | RemoveEncryptionKey
    | UpdateEncryptionKey Json.Value
      -----------------------------------------
      -- 📭 Other
      -----------------------------------------
    | RefreshedDropboxTokens { currentTime : Int, refreshToken : String } Dropbox.Tokens Msg
