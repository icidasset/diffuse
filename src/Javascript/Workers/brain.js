//
// Brain
// 🧠
//
// This worker is responsible for everything non-UI.

importScripts("/vendor/music-metadata.js")

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



// Cache
// -----

app.ports.removeCache.subscribe(event => {
  deleteFromIndex({ key: event.tag }).catch(reportError(event))
})


app.ports.requestCache.subscribe(event => {
  getFromIndex({ key: event.tag })
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
  const dataPromise = (_ => {
    switch (event.tag) {
      case "AUTH_SECRET_KEY": return keyFromPassphrase(event.data)
      default: return Promise.resolve(event.data)
    }
  })()

  dataPromise
    .then(data => setInIndex({ key: event.tag, data: data }))
    .catch(reportError(event))
})



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
  const path = ipfsFilePath(event.tag)
  const secretKeyPromise = getFromIndex({ key: "AUTH_SECRET_KEY" })

  fetch("http://localhost:5001/api/v0/files/read?arg=" + path)
    .then(r => r.ok ? r.text() : r.json())
    .then(r => secretKeyPromise.then(s => [r, s]))
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
  const json = JSON.stringify(event.data)
  const params = new URLSearchParams({
    arg: ipfsFilePath(event.tag),
    create: true,
    offset: 0,
    parents: true,
    truncate: true
  }).toString()

  getFromIndex({ key: "AUTH_SECRET_KEY" })
    .then(secretKey => encrypt(secretKey, json))
    .then(data => {
      const formData = new FormData()

      formData.append("data", data)

      return fetch(
        "http://localhost:5001/api/v0/files/write?" + params,
        { method: "POST", body: formData }
      )

    }).catch(
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



// 🔱
// --

function reportError(event) {
  return err => {
    console.error(err)
    app.ports.fromAlien.send({ tag: event.tag, data: null, error: err.message })
  }
}
