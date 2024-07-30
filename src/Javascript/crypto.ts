//
// Cryptography
// \ (•◡•) /
//
// Data encryption & decryption.


import * as Uint8arrays from "uint8arrays"


const extractable = false


export async function keyFromPassphrase(passphrase: string): Promise<CryptoKey> {
  const baseKey = await crypto.subtle.importKey(
    "raw",
    Uint8arrays.fromString(passphrase, "utf8"),
    {
      name: "PBKDF2"
    },
    false,
    [ "deriveKey" ]
  )

  return await crypto.subtle.deriveKey(
    {
      name: "PBKDF2",
      salt: Uint8arrays.fromString("diffuse", "utf8"),
      iterations: 10000,
      hash: "SHA-512"
    },
    baseKey,
    {
      name: "AES-GCM",
      length: 256
    },
    extractable,
    [ "encrypt", "decrypt" ]
  )
}


export async function encrypt(key: CryptoKey, string: string): Promise<string> {
  const iv = crypto.getRandomValues(new Uint8Array(12))

  const buf = await crypto.subtle.encrypt(
    {
      name: "AES-GCM",
      iv: iv,
      tagLength: 128
    },
    key,
    Uint8arrays.fromString(string, "utf8")
  )

  const iv_b64 = Uint8arrays.toString(iv, "base64pad")
  const buf_b64 = Uint8arrays.toString(new Uint8Array(buf), "base64pad")
  return iv_b64 + buf_b64
}


export async function decrypt(key: CryptoKey, string: string): Promise<string> {
  const iv_b64 = string.substring(0, 16)
  const buf_b64 = string.substring(16)

  const iv = Uint8arrays.fromString(iv_b64, "base64pad")
  const buf = Uint8arrays.fromString(buf_b64, "base64pad")

  const decrypted = await crypto.subtle.decrypt(
    {
      name: "AES-GCM",
      iv: iv,
      tagLength: 128
    },
    key,
    buf
  )

  return Uint8arrays.toString(
    new Uint8Array(decrypted),
    "utf8"
  )
}
