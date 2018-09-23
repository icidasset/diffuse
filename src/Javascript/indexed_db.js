//
// IndexedDB
// \ (•◡•) /
//
// The local database.
// This is used instead of localStorage.

importScripts("/vendor/text-encoding.js")


const indexedDB =
  self.indexedDB ||
  self.webkitIndexedDB ||
  self.mozIndexedDB ||
  self.msIndexedDB


const storeName =
  "main"


let db, idx, tries = 0


idx = indexedDB.open("diffuse", 1)
idx.onupgradeneeded = event => {
  event.target.result.createObjectStore(storeName)
}

idx.onsuccess = _ => {
  db = idx.result
}

idx.onerror = err => {
  console.error("IndexedDB error: " + err)
}



// Get
// ---

function getFromIndex(args) {
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
    const key = args.key
    const tra = db.transaction([storeName], "readwrite")
    const req = tra.objectStore(storeName).get(key)

    req.onsuccess = _ => {
      if (req.result) {
        resolve(args.useArrayBuffer ? arrayBufToString(req.result) : req.result)
      } else {
        resolve(null)
      }
    }

    req.onerror = reject
  })
}



// Set
// ---

function setInIndex(args) {
  return new Promise((resolve, reject) => {
    const key = args.key
    const dat = args.useArrayBuffer ? stringToArrayBuf(args.data) : args.data

    const tra = db.transaction([storeName], "readwrite")
    const req = tra.objectStore(storeName).put(dat, key)

    req.onsuccess = resolve
    req.onerror = reject
  })
}



// Delete
// ------

function deleteFromIndex(args) {
  return new Promise((resolve, reject) => {
    const key = args.key
    const tra = db.transaction([storeName], "readwrite")
    const req = tra.objectStore(storeName).delete(key)

    req.onsuccess = resolve
    req.onerror = reject
  })
}



// 🖍


function arrayBufToString(buf) {
  return new TextDecoder("utf-8").decode(new Uint8Array(buf))
}


function stringToArrayBuf(str) {
  return new TextEncoder().encode(str).buffer
}
