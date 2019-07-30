module String.Ext exposing (chopEnd, chopStart, prepend)

-- 🔱


chopEnd : String -> String -> String
chopEnd needle str =
    if String.endsWith needle str then
        str
            |> String.dropRight (String.length needle)
            |> chopEnd needle

    else
        str


chopStart : String -> String -> String
chopStart needle str =
    if String.startsWith needle str then
        str
            |> String.dropLeft (String.length needle)
            |> chopStart needle

    else
        str


{-| Flipped version of `append`.
-}
prepend : String -> String -> String
prepend a b =
    String.append b a
