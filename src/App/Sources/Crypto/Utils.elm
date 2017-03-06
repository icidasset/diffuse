module Sources.Crypto.Utils exposing (..)

{-| Utils.

A string is a Unicode string.
-}

import Char
import Sources.Crypto.Hex exposing (..)


stringToByteArray : String -> List Int
stringToByteArray =
    String.toList >> List.map Char.toCode


byteArrayToString : List Int -> String
byteArrayToString =
    List.map Char.fromCode >> String.fromList
