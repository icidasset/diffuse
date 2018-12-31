module Crypto.HMAC exposing (encrypt128, encrypt64)

{-| Cryptography – HMAC
-}

import Binary exposing (Bits)
import Crypto.Binary as Binary
import SHA


type alias HashFunction =
    String -> Bits


{-| HMAC encryption for hashing algorithms with a `blockSize` of 64.
These include: SHA-0, SHA-1, SHA-224, SHA-256, MD5, etc.

    >>> import Binary
    >>> import Crypto.Binary as Binary
    >>> import SHA

    >>> encrypt64 SHA.sha256 "" ""
    ..>   |> Binary.toHex
    ..>   |> String.toLower
    "b613679a0814d9ec772f95d778c35fc5ff1697c493715653c6c712144292c5ad"

    >>> encrypt64 SHA.sha256 "The quick brown fox jumps over the lazy dog" "key"
    ..>   |> Binary.toHex
    ..>   |> String.toLower
    "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8"

    >>> "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"
    ..>   |> Binary.fromHex
    ..>   |> Binary.toString
    ..>   |> encrypt64 SHA.sha256 "Hi There"
    ..>   |> Binary.toHex
    ..>   |> String.toLower
    "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"

-}
encrypt64 : HashFunction -> String -> String -> Bits
encrypt64 =
    encrypt 64


{-| HMAC encryption for hashing algorithms with a `blockSize` of 128.
These include: SHA-384, SHA-512, etc.
-}
encrypt128 : HashFunction -> String -> String -> Bits
encrypt128 =
    encrypt 128



-- ENCRYPT


encrypt : Int -> HashFunction -> String -> String -> Bits
encrypt blockSize hash message key =
    let
        keySize =
            String.length key

        keyWithBlockSize =
            if keySize > blockSize then
                hash key

            else if keySize < blockSize then
                Binary.fromString <| String.padRight blockSize (Char.fromCode 0) key

            else
                Binary.fromString <| key

        ( binSeqOne, binSeqTwo ) =
            keyWithBlockSize
                |> Binary.chunksOf 8
                |> List.map
                    (\k ->
                        ( Binary.xor k (Binary.fromDecimal 0x36)
                        , Binary.xor k (Binary.fromDecimal 0x5C)
                        )
                    )
                |> List.unzip
                |> Tuple.mapBoth Binary.concat Binary.concat
    in
    message
        |> Binary.fromString
        |> Binary.append binSeqOne
        |> Binary.toString
        |> hash
        |> Binary.append binSeqTwo
        |> Binary.toString
        |> hash
