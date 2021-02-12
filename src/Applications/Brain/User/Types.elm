module Brain.User.Types exposing (..)

import Debouncer.Basic as Debouncer
import Json.Decode as Json
import User.Layer as User exposing (HypaethralBit(..))
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
    | RetrieveHypaethralData HypaethralBit
    | HypaethralDataRetrieved Json.Value
      -----------------------------------------
      -- 2. Data (Legacy)
      -----------------------------------------
    | RetrieveLegacyHypaethralData
      -----------------------------------------
      -- x. Data
      -----------------------------------------
    | RetrieveEnclosedData
    | EnclosedDataRetrieved Json.Value
    | SaveEnclosedData Json.Value
    | SaveHypaethralData HypaethralBit Json.Value
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
    | SaveHypaethralDataBits (List HypaethralBit)
    | SaveHypaethralDataSlowly (Debouncer.Msg HypaethralBit)
    | SaveNextHypaethralBit
      -----------------------------------------
      -- z. Secret Key
      -----------------------------------------
    | RemoveEncryptionKey
    | UpdateEncryptionKey Json.Value
