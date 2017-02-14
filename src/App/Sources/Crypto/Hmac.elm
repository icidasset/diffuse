module Sources.Crypto.Hmac exposing (encrypt64, encrypt128)

{-| Cryptography
    – HMAC
-}

import Bitwise
import Sources.Crypto.Types exposing (..)
import Utils


{-| HMAC encryption for hashing algorithms with a `blockSize` of 64.
These include: SHA-0, SHA-1, SHA-224, SHA-256, MD5, etc.
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
        givenKeySize =
            String.length key

        keyWithCorrectSize =
            if givenKeySize > blockSize then
                hasher key
            else if givenKeySize < blockSize then
                String.padRight blockSize '0' key
            else
                key

        firstPart =
            String.repeat blockSize "\\"
                |> Utils.hexFromString
                |> Bitwise.xor (Utils.hexFromString keyWithCorrectSize)
                |> Utils.hexToString

        secondPart =
            String.repeat blockSize "6"
                |> Utils.hexFromString
                |> Bitwise.xor (Utils.hexFromString keyWithCorrectSize)
                |> Utils.hexToString
    in
        {-
           Basicly:
           1. Combine the second part and the message
           2. Send that to the hasher function (e.g. run through SHA-256 hashing algorithm)
           3. Combine that with the first part (i.e. append the hasher-function result)
           4. Send that again to the hasher function
           5. Success 🤘
        -}
        message
            |> String.append secondPart
            |> hasher
            |> String.append firstPart
            |> hasher
