module Sources.Utils exposing (..)

import Sources.Encoding exposing (..)
import Sources.Types exposing (..)


makeSource : SourceData -> Source
makeSource data =
    { id = "change_me_please"
    , data = data
    }


makeSourceReplica : Source -> SourceReplica
makeSourceReplica source =
    { id = source.id
    , data = encode source.data
    , typ = dataType source.data
    }


purify : SourceReplica -> Source
purify replica =
    { id = replica.id
    , data = decode replica.typ replica.data
    }
