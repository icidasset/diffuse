module String.Ext exposing (chopEnd, chopStart)


chopEnd : String -> String -> String
chopEnd needle str =
    if String.endsWith needle str then
        str
            |> String.dropRight 1
            |> chopEnd needle

    else
        str


chopStart : String -> String -> String
chopStart needle str =
    if String.startsWith needle str then
        str
            |> String.dropLeft 1
            |> chopStart needle

    else
        str
