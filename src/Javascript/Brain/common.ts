//
// Common stuff
// ʕ•ᴥ•ʔ


import * as crypto from "../crypto"
import * as db from "../indexed-db"


export const SECRET_KEY_LOCATION = "SECRET_KEY"


// 🔱


export function isLocalHost(url) {
  return (
    url.startsWith("localhost") ||
    url.startsWith("localhost") ||
    url.startsWith("127.0.0.1") ||
    url.startsWith("127.0.0.1")
  )
}


export function parseJsonIfNeeded(a) {
  if (typeof a === "string") return JSON.parse(a)
  return a
}


export function reportError(app, event) {
  return e => {
    const err = e ? e.message || e : null
    if (err) {
      console.error(err, e.stack)
      app.ports.fromAlien.send({ tag: event.tag, data: null, error: err })
    }
  }
}


export function sendData(app, event, opts) {
  return data => {
    app.ports.fromAlien.send({
      tag: event.tag,
      data: (opts && opts.parseJSON && typeof data === "string")
        ? JSON.parse(data)
        : (data || null),
      error: null
    })
  }
}



// Cache
// -----

export function removeCache(key) {
  return db.deleteFromIndex({ key: key })
}


export function fromCache(key) {
  return db.getFromIndex({ key: key })
}


export function toCache(key, data) {
  return db.setInIndex({ key: key, data: data }).then(result => {
    return result
  })
}



// Crypto
// ------

export function decryptIfNeeded(data) {
  if (typeof data !== "string") {
    return Promise.resolve(data)

  } else if (data.startsWith("{") || data.startsWith("[")) {
    return Promise.resolve(data)

  } else if (data.length < 15 && Number.isInteger(parseInt(data, 10))) {
    return Promise.resolve(data)

  } else {
    return data
      ? getSecretKey().then(secretKey => {
        if (!secretKey) throw new Error("There seems to be existing data that's encrypted, I will need the passphrase (ie. encryption key) to continue.")
        return crypto.decrypt(secretKey, data)
      })
      : Promise.resolve(null)

  }
}


export async function encryptIfPossible(unencryptedData: string): Promise<string> {
  return unencryptedData
    ? getSecretKey()
      .then(secretKey => crypto.encrypt(secretKey, unencryptedData))
      .catch(_ => unencryptedData)
    : unencryptedData
}


export { encryptIfPossible as encryptWithSecretKey }


export function getSecretKey() {
  return db.getFromIndex({ key: SECRET_KEY_LOCATION })
}
