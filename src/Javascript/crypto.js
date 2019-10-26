//
// Cryptography
// \ (•◡•) /
//
// Data encryption & decryption.


const crypto = (self.crypto || self.msCrypto)
const extractable = false


export function keyFromPassphrase(passphrase) {
  return crypto.subtle.importKey(
    "raw",
    stringToArrayBuffer(passphrase),
    {
      name: "PBKDF2"
    },
    false,
    [ "deriveKey" ]

  ).then(baseKey => crypto.subtle.deriveKey(
    {
      name: "PBKDF2",
      salt: stringToArrayBuffer("diffuse"),
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

  ))
}


export function encrypt(key, string) {
  let iv = crypto.getRandomValues(new Uint8Array(12))

  return crypto.subtle.encrypt(
    {
      name: "AES-GCM",
      iv: iv,
      tagLength: 128
    },
    key,
    stringToArrayBuffer(string)

  ).then(buf => {
    const iv_b64 = arrayBufferToBase64(iv)
    const buf_b64 = arrayBufferToBase64(buf)
    return iv_b64 + buf_b64

  })
}


export function decrypt(key, string) {
  const iv_b64 = string.substring(0, 16)
  const buf_b64 = string.substring(16)

  const iv = base64ToArrayBuffer(iv_b64)
  const buf = base64ToArrayBuffer(buf_b64)

  return crypto.subtle.decrypt(
    {
      name: "AES-GCM",
      iv: iv,
      tagLength: 128
    },
    key,
    buf

  ).then(
    arrayBufferToString

  )
}



// Buffers
// -------

function arrayBufferToBase64(buffer) {
  const uint8 = new Uint8Array(buffer)
  let string = ""

  for (let i = 0; i < uint8.byteLength; i++) {
    string = string + String.fromCharCode( uint8[i] )
  }

  return self.btoa(string)
}


function arrayBufferToString(buffer) {
  return new TextDecoder("UTF-8").decode(buffer)
}


function base64ToArrayBuffer(base64) {
  const string = self.atob(base64)
  const bytes = new Uint8Array(string.length)

  for (let i = 0; i < string.length; i++) {
    bytes[i] = string.charCodeAt(i)
  }

  return bytes.buffer
}


function stringToArrayBuffer(string) {
  return new TextEncoder("UTF-8").encode(string).buffer
}
