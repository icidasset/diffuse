module Alfred exposing (..)

-- 🌳


type alias Alfred action =
    { action : { result : Maybe String, searchTerm : Maybe String } -> List action
    , focus : Int
    , index : List String
    , message : String
    , operation : Operation
    , results : List String
    , searchTerm : Maybe String
    }


type Operation
    = Query
    | QueryOrMutation
    | Mutation
