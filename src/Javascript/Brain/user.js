//
// User
// (づ｡◕‿‿◕｡)づ
//
// Related to the user layer.


import * as crypto from "../crypto"
import { identity } from "../common"
import Textile from "../textile"

import { SECRET_KEY_LOCATION, decryptIfNeeded, encryptWithSecretKey } from "./common"
import { fromCache, isLocalHost, removeCache, reportError } from "./common"
import { sendJsonData, storageCallback, toCache } from "./common"


const ports = []


// Crypto
// ======

ports.fabricateSecretKey = app => event => {
  crypto.keyFromPassphrase(event.data)
    .then(data => toCache(SECRET_KEY_LOCATION, data))
    .then(_ => {
      app.ports.fromAlien.send({
        tag: event.tag,
        data: null,
        error: null
      })
    })
    .catch(reportError(app, event))
}



// Blockstack
// ----------

let bl


function bl0ckst4ck() {
  if (!bl) {
    importScripts("vendor/blockstack.min.js")

    bl = new blockstack.UserSession({
      appConfig: new blockstack.AppConfig({
        appDomain: location.origin
      }),
      sessionStore: BLOCKSTACK_SESSION_STORE
    })
  }

  return bl
}


const BLOCKSTACK_SESSION_STORE = {
  key: "AUTH_BLOCKSTACK_SESSION",
  getSessionData() { return fromCache(this.key).then(a => a || {}) },
  setSessionData(data) { return toCache(this.key, data) },
  deleteSessionData() { return removeCache(this.key) }
}


ports.deconstructBlockstack = _app => _ => {
  BLOCKSTACK_SESSION_STORE.deleteSessionData()
  bl = null
}


ports.handlePendingBlockstackSignIn = app => authResponse => {
  const session = bl0ckst4ck()

  session.handlePendingSignIn(authResponse).then(_ => {
    app.ports.fromAlien.send({
      tag: "SIGN_IN",
      data: { method: "BLOCKSTACK", passphrase: null },
      error: null
    })

  }).catch(
    reportError({ tag: "AUTH_BLOCKSTACK" })

  )
}


ports.redirectToBlockstackSignIn = app => event => {
  const session = bl0ckst4ck()

  session.generateAndStoreTransitKey().then(transitKey => {
    const dir = location.pathname.replace("brain.js", "")

    return session.makeAuthRequest(
      transitKey,
      location.origin + dir + "?action=authenticate/blockstack",
      location.origin + dir + "manifest.json",
      [ "store_write" ]
    )

  }).then(authRequest => {
    self.postMessage({
      action: "REDIRECT_TO_BLOCKSTACK",
      data: authRequest
    })

  }).catch(
    reportError(app, event)

  )
}


ports.requestBlockstack = app => event => {
  const session = bl0ckst4ck()

  session
    .getFile(event.data.file)
    .then( sendJsonData(app, event) )
    .catch( reportError(app, event) )
}


ports.toBlockstack = app => event => {
  const json = JSON.stringify(event.data.data)
  const session = bl0ckst4ck()

  session
    .putFile(event.data.file, json)
    .then( storageCallback(app, event) )
    .catch( reportError(app, event) )
}



// Dropbox
// -------

ports.requestDropbox = app => event => {
  const params = {
    path: "/" + event.data.file
  }

  const dataPromise =
    !navigator.onLine
    ? fromCache(event.tag + "_" + event.data.file)
    : fetch("https://content.dropboxapi.com/2/files/download", {
        method: "POST",
        headers: {
          "Authorization": "Bearer " + event.data.token,
          "Dropbox-API-Arg": JSON.stringify(params)
        }
      })
        .then(r => r.ok ? r.text() : r.json())
        .then(r => r.error ? null : r)
        .then(decryptIfNeeded)

  dataPromise
    .then( sendJsonData(app, event) )
    .catch( reportError(app, event) )
}


ports.toDropbox = app => event => {
  const json = JSON.stringify(event.data.data)
  const reporter = reportError(app, event)
  const params = {
    path: "/" + event.data.file,
    mode: "overwrite",
    mute: true
  }

  navigator.onLine && encryptWithSecretKey(json)
    .then(data => {
      return fetch("https://content.dropboxapi.com/2/files/upload", {
        method: "POST",
        headers: {
          "Authorization": "Bearer " + event.data.token,
          "Content-Type": "application/octet-stream",
          "Dropbox-API-Arg": JSON.stringify(params)
        },
        body: data
      })
    })
    .then( storageCallback(app, event) )
    .catch(reporter)

  toCache(event.tag + "_" + event.data.file, event.data.data)
    .then( !navigator.onLine ? storageCallback(app, event) : identity )
    .catch(reporter)
}



// IPFS
// ----

const IPFS_ROOT = "/Applications/Diffuse/"


ports.requestIpfs = app => event => {
  const apiOrigin = event.data.apiOrigin
  const path = IPFS_ROOT + event.data.file

  fetch(apiOrigin + "/api/v0/files/read?arg=" + path, { method: "POST" })
    .then(r => r.ok ? r.text() : r.json())
    .then(r => r.Code === 0 ? null : r)
    .then(decryptIfNeeded)
    .then( sendJsonData(app, event) )
    .catch( reportError(app, event) )
}


ports.toIpfs = app => event => {
  const apiOrigin = event.data.apiOrigin
  const json = JSON.stringify(event.data.data)
  const params = new URLSearchParams({
    arg: IPFS_ROOT + event.data.file,
    create: true,
    offset: 0,
    parents: true,
    truncate: true
  }).toString()

  encryptWithSecretKey(json)
    .then(data => {
      const formData = new FormData()

      formData.append("data", data)

      return fetch(
        apiOrigin + "/api/v0/files/write?" + params,
        { method: "POST", body: formData }
      )
    })
    .then( storageCallback(app, event) )
    .catch( reportError(app, event) )
}



// Legacy
// ------

ports.requestLegacyLocalData = app => event => {
  let oldIdx
  let key = location.hostname + ".json"

  oldIdx = indexedDB.open(key, 1)
  oldIdx.onsuccess = _ => {
    const old = oldIdx.result
    const tra = old.transaction([key], "readwrite")
    const req = tra.objectStore(key).get(key)

    req.onsuccess = _ => {
      if (req.result) sendJsonData(app, event)(req.result)
    }
  }
}



// Remote Storage
// --------------

let rs
let rsClient


function remoteStorage(event) {
  if (!rs) {
    importScripts("vendor/remotestorage.min.js")

    rs = new RemoteStorage({ cache: false })
    rs.access.claim("diffuse", "rw")

    rsClient = rs.scope("/diffuse/")

    return new Promise(resolve => {
      rs.on("connected", resolve)
      rs.connect(event.data.userAddress, event.data.token)
    })

  } else {
    return Promise.resolve()

  }
}


function remoteStorageIsUnavailable(event) {
  return !navigator.onLine &&
         !isLocalHost(event.data.userAddress.replace(/^[^@]*@/, ""))
}


ports.deconstructRemoteStorage = _app => _ => {
  rs = null
  rsClient = null
}


ports.requestRemoteStorage = app => event => {
  const isOffline =
    remoteStorageIsUnavailable(event)

  const dataPromise =
    isOffline
    ? fromCache(event.tag + "_" + event.data.file)
    : remoteStorage(event)
        .then(_ => rsClient.getFile(event.data.file))
        .then(r => r.data)
        .then(decryptIfNeeded)

  dataPromise
    .then( sendJsonData(app, event) )
    .catch( reportError(app, event) )
}


ports.toRemoteStorage = app => event => {
  const json = JSON.stringify(event.data.data)
  const doEncryption = _ => encryptWithSecretKey(json)
  const isOffline = remoteStorageIsUnavailable(event)

  !isOffline && remoteStorage(event)
    .then(doEncryption)
    .then(data => rsClient.storeFile("application/json", event.data.file, data))
    .then( storageCallback(app, event) )
    .catch( reportError(app, event) )

  toCache(event.tag + "_" + event.data.file, event.data.data)
    .then( isOffline ? storageCallback(app, event) : identity )
    .catch( reportError(app, event) )
}



// Textile
// -------


ports.requestTextile = app => event => {
  const apiOrigin = event.data.apiOrigin

  Textile.ensureThread
    (apiOrigin)

    .then(_ => Textile.getFile(apiOrigin, event.data.file))
    .then(f => f ? Textile.readFile(apiOrigin, f) : null)

    .then( sendJsonData(app, event) )
    .catch( reportError(app, event) )
}


ports.toTextile = app => event => {
  const apiOrigin = event.data.apiOrigin
  const json = JSON.stringify(event.data.data)

  Textile.ensureThread
    (apiOrigin)

    .then(_ => Textile.getFile(apiOrigin))
    .then(f => f ? Textile.deleteBlock(apiOrigin, f) : null)
    .then(_ => Textile.useMill(apiOrigin, event.data.file, json))
    .then(m => Textile.addFileToThread(apiOrigin, m))

    .then( storageCallback(app, event) )
    .catch( reportError(app, event) )
}



// EXPORT
// ======

export function setupPorts(app) {
  Object.keys(ports).forEach(name => {
    const fn = ports[name](app)
    app.ports[name].subscribe(fn)
  })
}
