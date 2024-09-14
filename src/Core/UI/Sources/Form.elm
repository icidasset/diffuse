module UI.Sources.Form exposing (..)

import Sources exposing (..)
import Sources.Services as Services
import UI.Sources.Types exposing (..)



-- 🌳


initialModel : Form
initialModel =
    { step = Where
    , context = defaultContext
    }


defaultContext : Source
defaultContext =
    { id = "CHANGE_ME_PLEASE"
    , data = Services.initialData defaultService
    , directoryPlaylists = True
    , enabled = True
    , service = defaultService
    }


defaultService : Service
defaultService =
    Dropbox
