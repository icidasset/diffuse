//
// IndexedDB
// \ (•◡•) /
//
// The local database.
// This is used instead of localStorage.


import delay from "delay"
import retryPromise from "p-retry"


const WAITING_MSG = "Waiting for database"


self.importScripts && importScripts("version.js")


const indexedDB =
  self.indexedDB ||
  self.webkitIndexedDB ||
  self.mozIndexedDB ||
  self.msIndexedDB


export const storeNames = {
  main: "main",
  tracks: "tracks"
}


let db, idx, tries = 0


idx = indexedDB.open("diffuse", 1)
idx.onupgradeneeded = event => {
  event.target.result.createObjectStore(storeNames.main)
  event.target.result.createObjectStore(storeNames.tracks)
}

idx.onsuccess = _ => {
  db = idx.result

  setInIndex({
    key: "VERSION",
    data: self.VERSION
  })
}

idx.onerror = event => {
  console.error("IndexedDB error: " + event.target.error)
}



// Get
// ---

export const getFromIndex = args => retry(() => {
  return new Promise((resolve, reject) => {
    if (!db) throw new Error(WAITING_MSG)

    const sto = args.store || storeNames.main
    const key = args.key
    const tra = db.transaction([sto], "readonly")
    const req = tra.objectStore(sto).get(key)

    req.onsuccess = _ => {
      if (req.result) {
        resolve(req.result)
      } else {
        resolve(null)
      }
    }

    req.onerror = reject
  })
})


export const keys = args => retry(() => {
  return new Promise((resolve, reject) => {
    if (!db) throw new Error(WAITING_MSG)

    const sto = (args || {}).store || storeNames.main
    const tra = db.transaction([sto], "readonly")
    const req = tra.objectStore(sto).getAllKeys()

    req.onsuccess = _ => {
      if (req.result) {
        resolve(req.result)
      } else {
        resolve(null)
      }
    }

    req.onerror = reject
  })
})



// Set
// ---

export const setInIndex = args => retry(() => {
  return new Promise((resolve, reject) => {
    if (!db) throw new Error(WAITING_MSG)

    const sto = args.store || storeNames.main
    const key = args.key
    const dat = args.data

    const tra = db.transaction([sto], "readwrite")
    const req = tra.objectStore(sto).put(dat, key)

    req.onsuccess = resolve
    req.onerror = reject
  })
})



// Delete
// ------

export const deleteFromIndex = args => retry(() => {
  return new Promise((resolve, reject) => {
    if (!db) throw new Error(WAITING_MSG)

    const sto = args.store || storeNames.main
    const key = args.key
    const tra = db.transaction([sto], "readwrite")
    const req = tra.objectStore(sto).delete(key)

    req.onsuccess = resolve
    req.onerror = reject
  })
})



// ⚗️
// --

function retry(func) {
  return retryPromise(func, { onFailedAttempt: _ => delay(250), retries: 20 })
}
