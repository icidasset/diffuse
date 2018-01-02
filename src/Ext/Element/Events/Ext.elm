module Element.Events.Ext exposing (preventDefaultOptions, stopPropagationOptions)

import Element.Events exposing (Options)


-- 🍯


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
