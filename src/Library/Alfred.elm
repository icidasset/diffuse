module Alfred exposing (..)

import Material.Icons.Types exposing (Coloring)
import Svg exposing (Svg)



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
    { icon : Maybe (Coloring -> Svg msg)
    , title : String
    , value : ItemValue msg
    }


type ItemValue msg
    = Command msg
    | StringValue String


type Operation
    = Query
    | QueryOrMutation
    | Mutation



-- 🛠


command : ItemValue msg -> Maybe msg
command val =
    case val of
        Command cmd ->
            Just cmd

        StringValue _ ->
            Nothing


stringValue : ItemValue msg -> Maybe String
stringValue val =
    case val of
        Command _ ->
            Nothing

        StringValue string ->
            Just string
