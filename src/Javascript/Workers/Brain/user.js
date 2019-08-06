//
// User
// (づ｡◕‿‿◕｡)づ
//
// Related to the user layer.


const SECRET_KEY_LOCATION = "AUTH_SECRET_KEY"


// Crypto
// ======

app.ports.fabricateSecretKey.subscribe(event => {
  keyFromPassphrase(event.data)
    .then(data => toCache(SECRET_KEY_LOCATION, data))
    .then(_ => {
      app.ports.fromAlien.send({
        tag: event.tag,
        data: null,
        error: null
      })
    })
    .catch(reportError(event))
})



// Blockstack
// ----------

let bl


function bl0ckst4ck() {
  if (!bl) {
    importScripts("../vendor/blockstack.min.js")

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


app.ports.deconstructBlockstack.subscribe(_ => {
  BLOCKSTACK_SESSION_STORE.deleteSessionData()
  bl = null
})


app.ports.handlePendingBlockstackSignIn.subscribe(authResponse => {
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
})


app.ports.redirectToBlockstackSignIn.subscribe(event => {
  const session = bl0ckst4ck()

  session.generateAndStoreTransitKey().then(transitKey => {
    const dir = location.pathname.replace("workers/brain.js", "")

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
    reportError(event)

  )
})


app.ports.requestBlockstack.subscribe(event => {
  const session = bl0ckst4ck()

  bl
    .getFile(event.data.file)
    .then( sendJsonData(event) )
    .catch( reportError(event) )
})


app.ports.toBlockstack.subscribe(event => {
  const json = JSON.stringify(event.data.data)
  const session = bl0ckst4ck()

  bl
    .putFile(event.data.file, json)
    .catch( reportError(event) )
})



// Dropbox
// -------

app.ports.requestDropbox.subscribe(event => {
  const params = {
    path: "/" + event.data.file
  }

  const dataPromise =
    !navigator.onLine
    ? fromCache(event.tag)
    : fetch("https://content.dropboxapi.com/2/files/download", {
        method: "POST",
        headers: {
          "Authorization": "Bearer " + event.data.token,
          "Dropbox-API-Arg": JSON.stringify(params)
        }
      })
        .then(r => r.ok ? r.text() : r.json())
        .then(r => r.error ? null : r)
        .then(decryptWithSecretKey)

  dataPromise
    .then( sendJsonData(event) )
    .catch( reportError(event) )
})


app.ports.toDropbox.subscribe(event => {
  const json = JSON.stringify(event.data.data)
  const reporter = reportError(event)
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
    .catch(reporter)

  toCache(event.tag, event.data.data)
    .catch(reporter)
})



// IPFS
// ----

const IPFS_ROOT = "/Applications/Diffuse/"


app.ports.requestIpfs.subscribe(event => {
  const apiOrigin = event.data.apiOrigin
  const path = IPFS_ROOT + event.data.file

  fetch(apiOrigin + "/api/v0/files/read?arg=" + path)
    .then(r => r.ok ? r.text() : r.json())
    .then(r => r.Code === 0 ? null : r)
    .then(decryptWithSecretKey)
    .then( sendJsonData(event) )
    .catch( reportError(event) )
})


app.ports.toIpfs.subscribe(event => {
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

    }).catch(
      reportError(event)

    )
})



// Legacy
// ------

app.ports.requestLegacyLocalData.subscribe(event => {
  let oldIdx
  let key = location.hostname + ".json"

  oldIdx = indexedDB.open(key, 1)
  oldIdx.onsuccess = _ => {
    const old = oldIdx.result
    const tra = old.transaction([key], "readwrite")
    const req = tra.objectStore(key).get(key)

    req.onsuccess = _ => {
      if (req.result) sendJsonData(event)(req.result)
    }
  }
})



// Remote Storage
// --------------

let rs
let rsClient


function remoteStorage(event) {
  if (!rs) {
    importScripts("../vendor/remotestorage.min.js")

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


app.ports.deconstructRemoteStorage.subscribe(_ => {
  rs = null
  rsClient = null
})


app.ports.requestRemoteStorage.subscribe(event => {
  const isOffline =
    remoteStorageIsUnavailable(event)

  const dataPromise =
    isOffline
    ? fromCache(event.tag)
    : remoteStorage(event)
        .then(_ => rsClient.getFile(event.data.file))
        .then(r => r.data)
        .then(decryptWithSecretKey)

  dataPromise
    .then( sendJsonData(event) )
    .then( reportError(event) )
})


app.ports.toRemoteStorage.subscribe(event => {
  const json = JSON.stringify(event.data.data)
  const doEncryption = _ => encryptWithSecretKey(json)
  const isOffline = remoteStorageIsUnavailable(event)

  !isOffline && remoteStorage(event)
    .then(doEncryption)
    .then(data => rsClient.storeFile("application/json", event.data.file, data))
    .catch( reportError(event) )

  toCache(event.tag, event.data.data)
    .catch( reportError(event) )
})



// Textile
// -------

let tt


function textile() {
  if (!tt) {
    importScripts("../textile.js")
    tt = true
  }
}


app.ports.requestTextile.subscribe(event => {
  const apiOrigin = event.data.apiOrigin

  textile()

  Textile.ensureThread
    (apiOrigin)

    .then(_ => Textile.getFile(apiOrigin, event.data.file))
    .then(f => f ? Textile.readFile(apiOrigin, f) : null)

    .then( sendJsonData(event) )
    .then( reportError(event) )
})


app.ports.toTextile.subscribe(event => {
  const apiOrigin = event.data.apiOrigin
  const json = JSON.stringify(event.data.data)

  textile()

  Textile.ensureThread
    (apiOrigin)

    .then(_ => Textile.getFile(apiOrigin))
    .then(f => f ? Textile.deleteBlock(apiOrigin, f) : null)
    .then(_ => Textile.useMill(apiOrigin, event.data.file, json))
    .then(m => Textile.addFileToThread(apiOrigin, m))

    .catch( reportError(event) )
})



// 🛠


function decryptWithSecretKey(encryptedData) {
  return encryptedData
    ? getSecretKey().then(secretKey => decrypt(secretKey, encryptedData))
    : null
}


function encryptWithSecretKey(unencryptedData) {
  return unencryptedData
    ? getSecretKey().then(secretKey => encrypt(secretKey, unencryptedData))
    : null
}


function getSecretKey() {
  return getFromIndex({
    key: SECRET_KEY_LOCATION
  }).then(key => {
    return key ? key : Promise.reject(new Error("MISSING_SECRET_KEY"))
  })
}


function isAuthMethodService(eventTag) {
  return (
    eventTag.startsWith("AUTH_") &&
    eventTag !== "AUTH_BLOCKSTACK_SESSION" &&
    eventTag !== "AUTH_ENCLOSED_DATA" &&
    eventTag !== "AUTH_METHOD" &&
    eventTag !== "AUTH_SECRET_KEY"
  )
}


function isLocalHost(url) {
  return (
    url.startsWith("localhost") ||
    url.startsWith("localhost") ||
    url.startsWith("127.0.0.1") ||
    url.startsWith("127.0.0.1")
  )
}


function sendData(event, opts) {
  return data => {
    app.ports.fromAlien.send({
      tag: event.tag,
      data: (opts && opts.parseJSON && typeof data === "string") ? JSON.parse(data) : data,
      error: null
    })
  }
}


function sendJsonData(event) {
  return sendData(event, { parseJSON: true })
}
