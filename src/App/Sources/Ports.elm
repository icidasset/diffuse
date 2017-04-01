port module Sources.Ports exposing (..)

import Sources.Types exposing (..)


-- 💡


port requestTags : ProcessingContextForTags -> Cmd msg



-- 🚽


port receiveTags : (ProcessingContextForTags -> msg) -> Sub msg
