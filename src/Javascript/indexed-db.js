//
// IndexedDB
// \ (•◡•) /
//
// The local database.
// This is used instead of localStorage.


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

export function getFromIndex(args) {
  if (!db && tries < 20) {
    tries++

    return new Promise((resolve, reject) => {
      setTimeout(
        () => { getFromIndex(args).then(resolve, reject) },
        250
      )
    })
  }

  tries = 0

  return new Promise((resolve, reject) => {
    const sto = args.store || storeNames.main
    const key = args.key
    const tra = db.transaction([sto], "readwrite")
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
}



// Set
// ---

export function setInIndex(args) {
  return new Promise((resolve, reject) => {
    const sto = args.store || storeNames.main
    const key = args.key
    const dat = args.data

    const tra = db.transaction([sto], "readwrite")
    const req = tra.objectStore(sto).put(dat, key)

    req.onsuccess = resolve
    req.onerror = reject
  })
}



// Delete
// ------

export function deleteFromIndex(args) {
  return new Promise((resolve, reject) => {
    const sto = args.store || storeNames.main
    const key = args.key
    const tra = db.transaction([sto], "readwrite")
    const req = tra.objectStore(sto).delete(key)

    req.onsuccess = resolve
    req.onerror = reject
  })
}
