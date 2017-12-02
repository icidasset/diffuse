module Sources.Crypto.Hmac exposing (encrypt64, encrypt128)

{-| Cryptography
||| – HMAC

    Currently it only works with hash functions
    that take hex-encoded strings.

    Most of these hashing functions in Elm take a `String` as argument,
    which they then convert to single-byte UTF-8 (or something like that).
    If we decode the output of the hashing function we won't get single-byte UTF-8,
    but another form of Unicode. WHICH IS PROBABLY THE REASON WE CAN'T PASS IT
    AGAIN TO THE HASHING FUNCTION.

    Disclaimer: The above may not be true at all, but that's what I assume.

-}

import Bitwise
import Char
import Sources.Crypto.Hex exposing (..)
import Sources.Crypto.Types exposing (..)
import Sources.Crypto.Utils exposing (..)


{-| HMAC encryption for hashing algorithms with a `blockSize` of 64.
These include: SHA-0, SHA-1, SHA-224, SHA-256, MD5, etc.

    >>> encrypt64 SHA.sha256sum "The quick brown fox jumps over the lazy dog" "key"
    >>>     |> unicodeToHex 2
    "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8"

    >>> encrypt64 SHA.sha256sum "" ""
    >>>     |> unicodeToHex 2
    "b613679a0814d9ec772f95d778c35fc5ff1697c493715653c6c712144292c5ad"

-}
encrypt64 : HashFunction -> String -> String -> String
encrypt64 =
    encrypt 64


{-| HMAC encryption for hashing algorithms with a `blockSize` of 128.
These include: SHA-384, SHA-512, etc.
-}
encrypt128 : HashFunction -> String -> String -> String
encrypt128 =
    encrypt 128



-- Encrypt


encrypt : Int -> HashFunction -> String -> String -> String
encrypt blockSize hasher message key =
    let
        keySize =
            String.length key

        hash =
            unicodeToHex 2
                >> String.append "0x"
                >> hasher
                >> unicodeHexToUnicode

        keyWithBlockSize =
            if keySize > blockSize then
                hash key
            else if keySize < blockSize then
                String.padRight blockSize (Char.fromCode 0) key
            else
                key

        ( byteArrayOne, byteArrayTwo ) =
            keyWithBlockSize
                |> stringToByteArray
                |> List.map (\k -> ( Bitwise.xor k 54, Bitwise.xor k 92 ))
                |> List.unzip
    in
        message
            |> String.append (byteArrayToString byteArrayOne)
            |> hash
            |> String.append (byteArrayToString byteArrayTwo)
            |> hash
