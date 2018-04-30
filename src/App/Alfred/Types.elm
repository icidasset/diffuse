module Alfred.Types exposing (..)

import Keyboard.Extra as Keyboard


-- Messages


type Msg parent
    = Assign (Alfred parent)
    | CalculateResults String
    | Hide
    | RunAction Int
      -- Keyboard
    | KeydownMsg Keyboard.Key



-- Model


type alias Model parentMsg =
    { instance : Maybe (Alfred parentMsg)
    }



-- Alfred


type alias Alfred msg =
    { action : Maybe String -> Maybe String -> Cmd msg
    , focus : Int
    , index : List String
    , message : String
    , results : List String
    , searchTerm : Maybe String
    }
