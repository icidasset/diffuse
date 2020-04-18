module Alfred exposing (Alfred)

-- 🌳


type alias Alfred action =
    { action : { result : Maybe String, searchTerm : Maybe String } -> List action
    , focus : Int
    , index : List String
    , message : String
    , results : List String
    , searchTerm : Maybe String
    }
