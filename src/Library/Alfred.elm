module Alfred exposing (..)

import List.Extra as List
import Material.Icons.Types exposing (Coloring)
import Svg exposing (Svg)



-- 🌳


type alias Alfred msg =
    { action : Action msg
    , focus : Int
    , index : List (Group msg)
    , indexFlattened : List (Item msg)
    , message : String
    , operation : Operation
    , results : List (Group msg)
    , searchTerm : Maybe String
    }


type alias Action msg =
    { result : Maybe (Item msg), searchTerm : Maybe String } -> List msg


type alias Group msg =
    { name : Maybe String, items : List (Item msg) }


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



-- 🛳


create :
    { action : Action msg
    , index : List (Group msg)
    , message : String
    , operation : Operation
    }
    -> Alfred msg
create { action, index, message, operation } =
    { action = action
    , focus = 0
    , index = index
    , indexFlattened = List.concatMap .items index
    , message = message
    , operation = operation
    , results = index
    , searchTerm = Nothing
    }



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



-- 🛠


getAt : Int -> Alfred msg -> Maybe (Item msg)
getAt index alfred =
    alfred.results
        |> List.concatMap .items
        |> List.getAt index


length : Alfred msg -> Int
length { indexFlattened } =
    List.length indexFlattened
