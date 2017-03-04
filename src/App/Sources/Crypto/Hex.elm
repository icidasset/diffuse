module Sources.Crypto.Hex exposing (..)

{-| Cryptography
    – Hex
-}

import Char
import Hex
import List.Extra


{-| Convert Hex string to a UTF-8 string.

    >>> hexStringToUtf8String "c5738ffbaa1ff9d62e688841e89e608e"
    "ÅsûªùÖ.hAè`"
-}
hexStringToUtf8String : String -> String
hexStringToUtf8String input =
    input
        |> String.toList
        |> List.Extra.greedyGroupsOf 2
        |> List.map (String.fromList >> hexStringToUtf8Char)
        |> String.fromList


{-| Convert Hex string to a UTF-8 character.

    >>> hexStringToUtf8Char "a5"
    '¥'
-}
hexStringToUtf8Char : String -> Char
hexStringToUtf8Char input =
    case Hex.fromString input of
        Ok v ->
            Char.fromCode v

        Err err ->
            Debug.crash err
