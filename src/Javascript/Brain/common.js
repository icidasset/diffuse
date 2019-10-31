//
// Common stuff
// ʕ•ᴥ•ʔ


import * as crypto from "../crypto"
import * as db from "../indexed-db"


export const SECRET_KEY_LOCATION = "AUTH_SECRET_KEY"


// 🔱


export function isAuthMethodService(eventTag) {
  return (
    eventTag.startsWith("AUTH_") &&
    eventTag !== "AUTH_BLOCKSTACK_SESSION" &&
    eventTag !== "AUTH_ENCLOSED_DATA" &&
    eventTag !== "AUTH_METHOD" &&
    eventTag !== "AUTH_SECRET_KEY"
  )
}


export function isLocalHost(url) {
  return (
    url.startsWith("localhost") ||
    url.startsWith("localhost") ||
    url.startsWith("127.0.0.1") ||
    url.startsWith("127.0.0.1")
  )
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


export function sendJsonData(app, event) {
  return sendData(app, event, { parseJSON: true })
}


export function storageCallback(app, _) {
  return _ => {
    app.ports.savedHypaethralBit.send()
  }
}



// Cache
// -----

export function removeCache(key) {
  return db.deleteFromIndex({ key: key })
}


export function fromCache(key) {
  return isAuthMethodService(key)
    ? db.getFromIndex({ key: key })
        .then(decryptIfNeeded)
        .then(d => typeof d === "string" ? JSON.parse(d) : d)
        .then(a => a === undefined ? null : a)
    : db.getFromIndex({ key: key })
}


export function toCache(key, data) {
  if (isAuthMethodService(key)) {
    const json = JSON.stringify(data)

    return encryptWithSecretKey(json)
      .then(encryptedData => db.setInIndex({ key: key, data: encryptedData }))

  } else {
    return db.setInIndex({ key: key, data: data })

  }
}



// Crypto
// ------

export function decryptIfNeeded(data) {
  if (typeof data !== "string") {
    return Promise.resolve(data)

  } else if (data.startsWith("{") || data.startsWith("[")) {
    return Promise.resolve(data)

  } else {
    return data
      ? getSecretKey().then(secretKey => crypto.decrypt(secretKey, data))
      : Promise.resolve(null)

  }
}


export function encryptWithSecretKey(unencryptedData) {
  return unencryptedData
    ? getSecretKey()
        .then(secretKey => crypto.encrypt(secretKey, unencryptedData))
        .catch(_ => unencryptedData)
    : null
}


export function getSecretKey() {
  return db.getFromIndex({
    key: SECRET_KEY_LOCATION
  }).then(key => {
    return key ? key : Promise.reject(new Error("MISSING_SECRET_KEY"))
  })
}
