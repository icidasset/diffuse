module Sources.Crypto.Hmac exposing (encrypt64, encrypt128)

{-| Cryptography
    – HMAC
-}

import Bitwise
import Char
import Debug
import Sources.Crypto.Hex exposing (hexStringToUtf8String)
import Sources.Crypto.Types exposing (..)


{-| HMAC encryption for hashing algorithms with a `blockSize` of 64.
These include: SHA-0, SHA-1, SHA-224, SHA-256, MD5, etc.

    >>> import SHA
    >>> import MD5

    >>> encrypt64 SHA.sha256sum "The quick brown fox jumps over the lazy dog" "key"
    "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8"

    >>> encrypt64 SHA.sha256sum "" ""
    "b613679a0814d9ec772f95d778c35fc5ff1697c493715653c6c712144292c5ad"

    >>> encrypt64 MD5.hex "" ""
    "74e6f7298a9c2d168935f58c001bad88"
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



-- Private


encrypt : Int -> HashFunction -> String -> String -> String
encrypt blockSize hasher message key =
    let
        keySize =
            String.length key

        keyWithCorrectSize =
            if keySize > blockSize then
                hexStringToUtf8String (hasher key)
            else if keySize < blockSize then
                String.padRight blockSize (Char.fromCode 0) key
            else
                key

        keyCodePoints =
            keyWithCorrectSize
                |> String.toList
                |> List.map Char.toCode

        partA =
            keyCodePoints
                |> List.map (Bitwise.xor 54 >> Char.fromCode)
                |> String.fromList

        partB =
            keyCodePoints
                |> List.map (Bitwise.xor 92 >> Char.fromCode)
                |> String.fromList
    in
        message
            |> String.append partA
            |> hasher
            |> hexStringToUtf8String
            |> String.append partB
            |> hasher
