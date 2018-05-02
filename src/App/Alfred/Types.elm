module Alfred.Types exposing (..)

import Keyboard.Extra as Keyboard


-- Messages


type Msg msg
    = Assign (Alfred msg)
    | CalculateResults String
    | Hide
    | RunAction Int
      -- Keyboard
    | KeydownMsg Keyboard.Key



-- Model


type alias Model msg =
    { instance : Maybe (Alfred msg)
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
