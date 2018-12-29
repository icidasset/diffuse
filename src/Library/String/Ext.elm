module String.Ext exposing (chopEnd, chopStart)


chopEnd : String -> String -> String
chopEnd needle str =
    if String.endsWith needle str then
        str
            |> String.dropRight (String.length str)
            |> chopEnd needle

    else
        str


chopStart : String -> String -> String
chopStart needle str =
    if String.startsWith needle str then
        str
            |> String.dropLeft (String.length str)
            |> chopStart needle

    else
        str
