module Crypto.HMAC exposing (encrypt128, encrypt64)

{-| Cryptography – HMAC
-}

import Binary exposing (Bits)
import SHA


type alias HashFunction =
    Bits -> Bits


{-| HMAC encryption for hashing algorithms with a `blockSize` of 64.
These include: SHA-0, SHA-1, SHA-224, SHA-256, MD5, etc.

    >>> import Binary
    >>> import SHA

    >>> Binary.fromStringAsUtf8 ""
    ..>   |> encrypt64 SHA.sha256 ""
    ..>   |> Binary.toHex
    ..>   |> String.toLower
    "b613679a0814d9ec772f95d778c35fc5ff1697c493715653c6c712144292c5ad"

    >>> Binary.fromStringAsUtf8 "key"
    ..>   |> encrypt64 SHA.sha256 "The quick brown fox jumps over the lazy dog"
    ..>   |> Binary.toHex
    ..>   |> String.toLower
    "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8"

    >>> Binary.fromHex "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"
    ..>   |> encrypt64 SHA.sha256 "Hi There"
    ..>   |> Binary.toHex
    ..>   |> String.toLower
    "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"

-}
encrypt64 : HashFunction -> String -> Bits -> Bits
encrypt64 =
    encrypt (64 * 8)


{-| HMAC encryption for hashing algorithms with a `blockSize` of 128.
These include: SHA-384, SHA-512, etc.
-}
encrypt128 : HashFunction -> String -> Bits -> Bits
encrypt128 =
    encrypt (128 * 8)



-- ENCRYPT


encrypt : Int -> HashFunction -> String -> Bits -> Bits
encrypt blockSize hash messageString key =
    let
        keySize =
            Binary.width key

        keyWithBlockSize =
            if keySize > blockSize then
                hash key

            else if keySize < blockSize then
                False
                    |> List.repeat (blockSize - keySize)
                    |> List.append (Binary.toBooleans key)
                    |> Binary.fromBooleans

            else
                key

        ( binSeqOne, binSeqTwo ) =
            Tuple.mapBoth
                (Binary.xor keyWithBlockSize)
                (Binary.xor keyWithBlockSize)
                (padding <| blockSize // 8)
    in
    messageString
        |> Binary.fromString 8
        |> Binary.append binSeqOne
        |> hash
        |> Binary.append binSeqTwo
        |> hash


padding : Int -> ( Bits, Bits )
padding blockSize =
    ( Binary.concat (List.repeat blockSize <| Binary.fromHex "36")
    , Binary.concat (List.repeat blockSize <| Binary.fromHex "5C")
    )
