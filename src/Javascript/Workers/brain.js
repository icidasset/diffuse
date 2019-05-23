//
// Brain
// 🧠
//
// This worker is responsible for everything non-UI.

importScripts("/vendor/musicmetadata.min.js")

importScripts("/brain.js")
importScripts("/encryption.js")
importScripts("/indexed-db.js")
importScripts("/processing.js")
importScripts("/urls.js")


const app = Elm.Brain.init()



// UI
// ==

self.onmessage = event => {
  app.ports.fromAlien.send(event.data)
}

app.ports.toUI.subscribe(event => {
  self.postMessage(event)
})



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


function getSecretKey() {
  return getFromIndex({ key: SECRET_KEY_LOCATION })
}



// Cache
// -----

app.ports.removeCache.subscribe(event => {
  deleteFromIndex({ key: event.tag }).catch(reportError(event))
})


app.ports.requestCache.subscribe(event => {
  const dataPromise = (_ => {
    if (event.tag == "AUTH_ANONYMOUS") {
      return getFromIndex({ key: event.tag })
        .then(r => getSecretKey().then(s => [r, s]))
        .then(([r, s]) => r ? decrypt(s, r) : null)
        .then(d => d ? JSON.parse(d) : null)

    } else {
      return getFromIndex({ key: event.tag })
    }
  })()

  dataPromise
    .then(data => {
      app.ports.fromAlien.send({
        tag: event.tag,
        data: data,
        error: null
      })
    }).catch(
      reportError(event)
    )
})


app.ports.toCache.subscribe(event => {
  toCache(event.tag, event.data).catch(reportError(event))
})


function toCache(key, data) {
  if (key == "AUTH_ANONYMOUS") {
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
      reportError(event)
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
      reportError(event)

    )
})



// Remote Storage
// --------------

let rs
let rsClient


function remoteStorage(event) {
  if (!rs) {
    importScripts("/vendor/remotestorage.min.js")

    rs = new RemoteStorage({ cache: false })
    rs.access.claim("diffuse", "rw")

    rsClient = rs.scope("/diffuse-v2/")

    return new Promise(resolve => {
      rs.on("connected", resolve)
      rs.connect(event.data.userAddress, event.data.token)
    })

  } else {
    return Promise.resolve()

  }
}


app.ports.requestRemoteStorage.subscribe(event => {
  remoteStorage(event)
    .then(_ => rsClient.getFile("diffuse.json"))
    .then(r => getSecretKey().then(s => [r.data, s]))
    .then(([r, s]) => r ? decrypt(s, r) : null)
    .then(data => {
      app.ports.fromAlien.send({
        tag: event.tag,
        data: typeof data === "string" ? JSON.parse(data) : data,
        error: null
      })
    })
    .catch(
      reportError(event)
    )
})


app.ports.toRemoteStorage.subscribe(event => {
  const json = JSON.stringify(event.data.data)

  remoteStorage(event)
    .then(_ => getSecretKey())
    .then(secretKey => encrypt(secretKey, json))
    .then(data => rsClient.storeFile("application/json", "diffuse.json", data))
    .catch(
      reportError(event)
    )
})



// Search
// ------

const search = new Worker("/workers/search.js")


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
    importScripts("/textile.js")
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

    .catch(reportError(event))
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

    .catch(reportError(event))
})



// 🔱
// --

function reportError(event) {
  return err => {
    console.error(err)
    app.ports.fromAlien.send({ tag: event.tag, data: null, error: err.message })
  }
}
