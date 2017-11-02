module Sources.Crypto.Hex exposing (..)

{-| Cryptography
||| – Hex
-}

import Char
import Hex
import Http
import List.Extra


{-| Convert a Unicode Hex string to a Unicode string.

    >>> unicodeHexToUnicode "c5738ffbaa1ff9d62e688841e89e608e"
    "ÅsûªùÖ.hAè`"

    >>> unicodeHexToUnicode "e620f820e520f1"
    "æ ø å ñ"

    >>> unicodeHexToUnicode "c3a620c3b820c3a520c3b1"
    "Ã¦ Ã¸ Ã¥ Ã±"

    >>> unicodeHexToUnicode "d83dde43"
    "Ø=ÞC"

-}
unicodeHexToUnicode : String -> String
unicodeHexToUnicode input =
    input
        |> String.toList
        |> List.Extra.greedyGroupsOf 2
        |> List.map (String.fromList >> hexToUnicodeChar)
        |> String.fromList


{-| Convert a UTF-8 Hex string to a Unicode string.

    >>> utf8HexToUnicode "c2a5"
    "¥"

    >>> utf8HexToUnicode "f09f9983"
    "🙃"

-}
utf8HexToUnicode : String -> String
utf8HexToUnicode input =
    input
        |> String.split ""
        |> List.Extra.greedyGroupsOf 2
        |> List.map (String.concat >> String.append "%")
        |> String.concat
        |> Http.decodeUri
        |> Maybe.withDefault ""


{-| Convert a UTF-16 Hex string to a Unicode string.

    >>> utf16HexToUnicode "d83dde43"
    "🙃"

-}
utf16HexToUnicode : String -> String
utf16HexToUnicode input =
    input
        |> String.toList
        |> List.Extra.greedyGroupsOf 4
        |> List.map (String.fromList >> hexToUnicodeChar)
        |> String.fromList


{-| Convert a Hex string to a Unicode character.

    >>> hexToUnicodeChar "a5"
    '¥'

    >>> hexToUnicodeChar "00a5"
    '¥'

-}
hexToUnicodeChar : String -> Char
hexToUnicodeChar input =
    case Hex.fromString input of
        Ok v ->
            Char.fromCode v

        Err err ->
            Debug.crash err


{-| Convert a Unicode string to a UTF-16 (kinda, depends on given padding) Hex string.

    >>> unicodeToHex 2 "ÅsûªùÖ.hAè`"
    "c5738ffbaa1ff9d62e688841e89e608e"

    >>> unicodeToHex 2 "æ ø å ñ"
    "e620f820e520f1"

    >>> unicodeToHex 2 "Ã¦ Ã¸ Ã¥ Ã±"
    "c3a620c3b820c3a520c3b1"

    >>> unicodeToHex 4 "🙃"
    "d83dde43"

    >>> let
    >>>     str =
    >>>         [0, 0, 0]
    >>>             |> List.map Char.fromCode
    >>>             |> String.fromList
    >>> in
    >>>     unicodeToHex 2 str
    "000000"

-}
unicodeToHex : Int -> String -> String
unicodeToHex padding input =
    input
        |> String.toList
        |> List.map (Char.toCode >> Hex.toString >> String.padLeft padding '0')
        |> String.concat
