module Crypto.Binary exposing (empty, fromString, toString)

import Binary exposing (Bits)


{-| Convert a string to binary.
Uses the UTF-8 text-encoding.

    >>> import Binary

    >>> "🤶"
    ..>   |> fromString
    ..>   |> Binary.toHex
    "1F936"

    >>> "abc"
    ..>   |> fromString
    ..>   |> Binary.toHex
    "616263"

-}
fromString : String -> Bits
fromString string =
    string
        |> String.toList
        |> List.map (Char.toCode >> Binary.fromDecimal >> Binary.ensureBits 8)
        |> Binary.concat


{-| Convert bits to a string.
Uses the UTF-8 text-encoding.

    >>> import Binary

    >>> "1F936"
    ..>   |> Binary.fromHex
    ..>   |> toString
    "🤶"

    >>> "616263"
    ..>   |> Binary.fromHex
    ..>   |> toString
    "abc"

-}
toString : Bits -> String
toString bits =
    bits
        |> Binary.chunksOf 8
        |> List.map (Binary.toDecimal >> Char.fromCode)
        |> String.fromList


{-| Empty binary sequence.
-}
empty : Bits
empty =
    Binary.fromBooleans []
