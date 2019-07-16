//
// Brain
// 🧠
//
// This worker is responsible for everything non-UI.

importScripts("../vendor/musicmetadata.min.js")
importScripts("../vendor/subworkers-polyfill.min.js")

importScripts("../brain.js")
importScripts("../encryption.js")
importScripts("../indexed-db.js")
importScripts("../processing.js")
importScripts("../urls.js")


const app = Elm.Brain.init()


function initialize(initialUrl) {
  app.ports.initialize.send(initialUrl)
}



// UI
// ==

self.onmessage = event => {
  if (event.data.action) return handleAction(event.data.action, event.data.data)
  if (event.data.tag) return app.ports.fromAlien.send(event.data)
}


app.ports.toUI.subscribe(event => {
  self.postMessage(event)
})


function handleAction(action, data) { switch (action) {
  case "INITIALIZE": return initialize(data)
}}



// Authentication
// --------------

const SECRET_KEY_LOCATION = "AUTH_SECRET_KEY"


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


function authError(event) {
  return reportError(event)
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
  getSessionData() { return fromCache(this.key) },
  setSessionData(data) { return toCache(this.key, data) },
  deleteSessionData() { return removeCache(this.key) }
}


app.ports.handlePendingBlockstackSignIn.subscribe(authResponse => {
  const session = bl0ckst4ck()

  console.log("TODO", authResponse)

  // TODO
  session.handlePendingSignIn(authResponse).then(userData => {
    console.log(userData)
  })
})


app.ports.redirectToBlockstackSignIn.subscribe(event => {
  const session = bl0ckst4ck()
  const authRequest = session.makeAuthRequest(
    session.generateAndStoreTransitKey(),
    location.origin + "?action=authenticate/blockstack",
    location.origin + "/manifest.json",
    [ "store_write" ]
  )

  self.postMessage({
    action: "REDIRECT_TO_BLOCKSTACK",
    data: authRequest
  })
})


app.ports.requestBlockstack.subscribe(event => {
  const session = bl0ckst4ck()
  // TODO
})


app.ports.toBlockstack.subscribe(event => {
  const json = JSON.stringify(event.data.data)
  const session = bl0ckst4ck()
  // TODO
})



// Cache
// -----

app.ports.removeCache.subscribe(event => {
  removeCache(event.tag).catch(reportError(event))
})


app.ports.removeTracksFromCache.subscribe(trackIds => {
  trackIds.reduce(
    (acc, id) => acc.then(_ => deleteFromIndex({ key: id, store: storeNames.tracks })),
    Promise.resolve()

  ).catch(
    _ => reportError
      ({ tag: "REMOVE_TRACKS_FROM_CACHE" })
      ("Failed to remove tracks from cache")

  )
})


app.ports.requestCache.subscribe(event => {
  fromCache(event.tag)
    .then(data => {
      app.ports.fromAlien.send({
        tag: event.tag,
        data: data,
        error: null
      })
    }).catch(
      isAuthMethodService(event.tag)
        ? authError(event)
        : reportError(event)
    )
})


app.ports.storeTracksInCache.subscribe(list => {
  list.reduce(
    (acc, item) => { return acc
      .then(_ => fetch(item.url))
      .then(r => r.blob())
      .then(b => setInIndex({ key: item.trackId, data: b, store: storeNames.tracks }))
    },
    Promise.resolve()

  ).then(
    // report success
    _ => self.postMessage({
      tag: "STORE_TRACKS_IN_CACHE",
      data: list.map(l => l.trackId),
      error: null
    })

  ).catch(
    _ => reportError
      ({ tag: "STORE_TRACKS_IN_CACHE" })
      ("Failed to store tracks in cache")

  )
})


app.ports.toCache.subscribe(event => {
  toCache(event.tag, event.data).catch(
    isAuthMethodService(event.tag)
      ? authError(event)
      : reportError(event)
  )
})


function fromCache(key) {
  if (isAuthMethodService(key)) {
    return getFromIndex({ key: key })
      .then(r => getSecretKey().then(s => [r, s]))
      .then(([r, s]) => typeof r === "string" ? decrypt(s, r) : r)
      .then(d => typeof d === "string" ? JSON.parse(d) : d)
      .then(a => a === undefined ? null : a)

  } else {
    return getFromIndex({ key: key })

  }
}


function removeCache(key) {
  return deleteFromIndex({ key: key })
}


function toCache(key, data) {
  if (isAuthMethodService(key)) {
    const json = JSON.stringify(data)

    return getSecretKey()
      .then(secretKey => encrypt(secretKey, json))
      .then(encryptedData => setInIndex({ key: key, data: encryptedData }))

  } else {
    return setInIndex({ key: key, data: data })

  }
}



// IPFS
// ----

const IPFS_ROOT = "/Applications/Diffuse/"


function ipfsFilePath(tag) {
  return IPFS_ROOT + (_ => {
    switch (tag) {
      case "AUTH_IPFS": return "Data.json"
      default: return tag
    }
  })()
}


app.ports.requestIpfs.subscribe(event => {
  const apiOrigin = event.data.apiOrigin
  const path = ipfsFilePath(event.tag)

  fetch(apiOrigin + "/api/v0/files/read?arg=" + path)
    .then(r => r.ok ? r.text() : r.json())
    .then(r => getSecretKey().then(s => [r, s]))
    .then(([r, s]) => r.Code === 0 ? {} : decrypt(s, r))
    .then(data => {
      app.ports.fromAlien.send({
        tag: event.tag,
        data: typeof data === "string" ? JSON.parse(data) : data,
        error: null
      })
    })
    .catch(
      authError(event)
    )
})


app.ports.toIpfs.subscribe(event => {
  const apiOrigin = event.data.apiOrigin
  const json = JSON.stringify(event.data.data)
  const params = new URLSearchParams({
    arg: ipfsFilePath(event.tag),
    create: true,
    offset: 0,
    parents: true,
    truncate: true
  }).toString()

  getSecretKey()
    .then(secretKey => encrypt(secretKey, json))
    .then(data => {
      const formData = new FormData()

      formData.append("data", data)

      return fetch(
        apiOrigin + "/api/v0/files/write?" + params,
        { method: "POST", body: formData }
      )

    }).catch(
      authError(event)

    )
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


app.ports.requestRemoteStorage.subscribe(event => {
  const dataPromise =
    !navigator.onLine &&
    !isLocalHost(event.data.userAddress.replace(/^[^@]*@/, ""))
    ? (
        fromCache(event.tag)
      )
    : (
        remoteStorage(event)
          .then(_ => rsClient.getFile("diffuse.json"))
          .then(r => getSecretKey().then(s => [r.data, s]))
          .then(([r, s]) => r ? decrypt(s, r) : null)
      )

  dataPromise
    .then(data => {
      app.ports.fromAlien.send({
        tag: event.tag,
        data: typeof data === "string" ? JSON.parse(data) : data,
        error: null
      })
    })
    .catch(
      authError(event)
    )
})


app.ports.toRemoteStorage.subscribe(event => {
  const json = JSON.stringify(event.data.data)

  remoteStorage(event)
    .then(_ => getSecretKey())
    .then(secretKey => encrypt(secretKey, json))
    .then(data => rsClient.storeFile("application/json", "diffuse.json", data))
    .catch(
      authError(event)
    )

  toCache(event.tag, event.data.data)
    .catch(
      reportError(event)
    )
})



// Search
// ------

const search = new Worker("search.js")


app.ports.requestSearch.subscribe(searchTerm => {
  search.postMessage({
    action: "PERFORM_SEARCH",
    data: searchTerm
  })
})


app.ports.updateSearchIndex.subscribe(tracksJson => {
  search.postMessage({
    action: "UPDATE_SEARCH_INDEX",
    data: tracksJson
  })
})


search.onmessage = event => {
  switch (event.data.action) {
    case "PERFORM_SEARCH":
      app.ports.receiveSearchResults.send(event.data.data)
      break
  }
}



// Tags
// ----

app.ports.requestTags.subscribe(context => {
  processContext(context).then(newContext => {
    app.ports.receiveTags.send(newContext)
  })
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

    .then(_ => Textile.getFile(apiOrigin))
    .then(f => f ? Textile.readFile(apiOrigin, f) : null)
    .then(data => {
      app.ports.fromAlien.send({
        tag: event.tag,
        data: typeof data === "string" ? JSON.parse(data) : data,
        error: null
      })
    })

    .catch(authError(event))
})


app.ports.toTextile.subscribe(event => {
  const apiOrigin = event.data.apiOrigin
  const json = JSON.stringify(event.data.data)

  textile()

  Textile.ensureThread
    (apiOrigin)

    .then(_ => Textile.getFile(apiOrigin))
    .then(f => f ? Textile.deleteBlock(apiOrigin, f) : null)
    .then(_ => Textile.useMill(apiOrigin, json))
    .then(m => Textile.addFileToThread(apiOrigin, m))

    .catch(authError(event))
})



// 🔱
// --

function reportError(event) {
  return err => {
    console.error(err)
    app.ports.fromAlien.send({ tag: event.tag, data: null, error: err.message })
  }
}
