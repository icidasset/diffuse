module Element.Events.Ext exposing (..)

import Element
import Element.Events exposing (Options)
import Json.Decode as Decode


-- 🍯


onClickStop : msg -> Element.Attribute variations msg
onClickStop msg =
    Element.Events.onWithOptions "click" stopPropagationOptions (Decode.succeed msg)


onClickPrevent : msg -> Element.Attribute variations msg
onClickPrevent msg =
    Element.Events.onWithOptions "click" preventDefaultOptions (Decode.succeed msg)


onClickStopAndPrevent : msg -> Element.Attribute variations msg
onClickStopAndPrevent msg =
    Element.Events.onWithOptions "click" stopAndPreventOptions (Decode.succeed msg)



-- Options


preventDefaultOptions : Options
preventDefaultOptions =
    { stopPropagation = False
    , preventDefault = True
    }


stopPropagationOptions : Options
stopPropagationOptions =
    { stopPropagation = True
    , preventDefault = False
    }


stopAndPreventOptions : Options
stopAndPreventOptions =
    { stopPropagation = True
    , preventDefault = True
    }
