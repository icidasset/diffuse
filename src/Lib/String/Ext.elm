module String.Ext exposing (..)


chop : String -> String -> String
chop needle str =
    if String.endsWith needle str then
        String.dropRight 1 str
    else
        str
