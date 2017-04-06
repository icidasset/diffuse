module Sources.Utils exposing (..)

import Sources.Types exposing (..)


makeSource : Service -> SourceData -> Source
makeSource service data =
    { id = "change_me_please"
    , data = data
    , service = service
    }
