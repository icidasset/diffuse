module Alfred exposing (..)

import Material.Icons.Types exposing (Icon)



-- 🌳


type alias Alfred msg =
    { action : Action msg
    , focus : Int
    , index : List (Item msg)
    , message : String
    , operation : Operation
    , results : List (Item msg)
    , searchTerm : Maybe String
    }


type alias Action msg =
    { result : Maybe (Item msg), searchTerm : Maybe String } -> List msg


type alias Item msg =
    { icon : Maybe (Icon msg)
    , title : String
    , value : String
    }


type Operation
    = Query
    | QueryOrMutation
    | Mutation
