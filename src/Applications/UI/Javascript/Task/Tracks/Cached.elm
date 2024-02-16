module UI.Javascript.Task.Tracks.Cached exposing (..)

import ConcurrentTask exposing (ConcurrentTask)
import Json.Decode as Decode
import Json.Encode as Encode
import UI.Javascript.Task.Types as JsTask


tracksCachedGet : { trackId : String } -> ConcurrentTask JsTask.Msg JsTask.Msg
tracksCachedGet { trackId } =
    ConcurrentTask.define
        { function = "tracks:cached:getBlobURL"
        , expect = ConcurrentTask.expectString
        , errors = ConcurrentTask.expectNoErrors
        , args = Encode.string trackId
        }
        |> ConcurrentTask.map GotCachedTrackBlobUrl
