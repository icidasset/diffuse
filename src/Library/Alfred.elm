module Alfred exposing (Alfred)

-- 🌳


type alias Alfred reply =
    { action : { result : Maybe String, searchTerm : Maybe String } -> List reply
    , focus : Int
    , index : List String
    , message : String
    , results : List String
    , searchTerm : Maybe String
    }
