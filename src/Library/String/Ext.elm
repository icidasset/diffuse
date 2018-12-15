module String.Ext exposing (chopEnd, chopStart)


chopEnd : String -> String -> String
chopEnd needle str =
    if String.endsWith needle str then
        String.dropRight (String.length str) str

    else
        str


chopStart : String -> String -> String
chopStart needle str =
    if String.startsWith needle str then
        String.dropLeft (String.length str) str

    else
        str
