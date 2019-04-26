module Binary.Ext exposing (fromBase64, intToBase64, toBase64)

import Binary exposing (Bits)



-- BASE64


fromBase64 : String -> Bits
fromBase64 encoded =
    Binary.empty


{-| To Base64.

    >>> import Binary

    >>> toBase64 (Binary.fromStringAsUtf8 "")
    ""

    >>> toBase64 (Binary.fromStringAsUtf8 "f")
    "Zg=="

    >>> toBase64 (Binary.fromStringAsUtf8 "fo")
    "Zm8="

    >>> toBase64 (Binary.fromStringAsUtf8 "foo")
    "Zm9v"

    >>> toBase64 (Binary.fromStringAsUtf8 "foob")
    "Zm9vYg=="

    >>> toBase64 (Binary.fromStringAsUtf8 "fooba")
    "Zm9vYmE="

    >>> toBase64 (Binary.fromStringAsUtf8 "foobar")
    "Zm9vYmFy"

-}
toBase64 : Bits -> String
toBase64 bits =
    bits
        |> Binary.chunksOf 6
        |> List.map (Binary.toDecimal >> intToBase64)
        |> String.fromList


intToBase64 : Int -> Char
intToBase64 i =
    case i of
        0 ->
            'A'

        1 ->
            'B'

        2 ->
            'C'

        3 ->
            'D'

        4 ->
            'E'

        5 ->
            'F'

        6 ->
            'G'

        7 ->
            'H'

        8 ->
            'I'

        9 ->
            'J'

        10 ->
            'K'

        11 ->
            'L'

        12 ->
            'M'

        13 ->
            'N'

        14 ->
            'O'

        15 ->
            'P'

        16 ->
            'Q'

        17 ->
            'R'

        18 ->
            'S'

        19 ->
            'T'

        20 ->
            'U'

        21 ->
            'V'

        22 ->
            'W'

        23 ->
            'X'

        24 ->
            'Y'

        25 ->
            'Z'

        26 ->
            'a'

        27 ->
            'b'

        28 ->
            'c'

        29 ->
            'd'

        30 ->
            'e'

        31 ->
            'f'

        32 ->
            'g'

        33 ->
            'h'

        34 ->
            'i'

        35 ->
            'j'

        36 ->
            'k'

        37 ->
            'l'

        38 ->
            'm'

        39 ->
            'n'

        40 ->
            'o'

        41 ->
            'p'

        42 ->
            'q'

        43 ->
            'r'

        44 ->
            's'

        45 ->
            't'

        46 ->
            'u'

        47 ->
            'v'

        48 ->
            'w'

        49 ->
            'x'

        50 ->
            'y'

        51 ->
            'z'

        52 ->
            '0'

        53 ->
            '1'

        54 ->
            '2'

        55 ->
            '3'

        56 ->
            '4'

        57 ->
            '5'

        58 ->
            '6'

        59 ->
            '7'

        60 ->
            '8'

        61 ->
            '9'

        62 ->
            '+'

        _ ->
            '/'
