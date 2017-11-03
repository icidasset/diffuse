port module Sources.Processing.Ports exposing (..)

import Sources.Processing.Types exposing (..)


-- 💡


port requestTags : ContextForTags -> Cmd msg



-- 🚽


port receiveTags : (ContextForTags -> msg) -> Sub msg
