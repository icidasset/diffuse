//
// Brain
// 🧠
//
// This worker is responsible for everything non-UI.

importScripts("../vendor/musicmetadata.min.js")
importScripts("../vendor/subworkers-polyfill.min.js")

importScripts("../brain.js")
importScripts("../crypto.js")
importScripts("../indexed-db.js")
importScripts("../processing.js")
importScripts("../urls.js")


const app = Elm.Brain.init()


function initialize(initialUrl) {
  app.ports.initialize.send(initialUrl)
}


importScripts("Brain/user.js")



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



// Cache
// -----

app.ports.removeCache.subscribe(event => {
  removeCache(event.tag)
    .catch( reportError(event) )
})


app.ports.requestCache.subscribe(event => {
  fromCache(event.tag)
    .then( sendData(event) )
    .catch( reportError(event) )
})


app.ports.toCache.subscribe(event => {
  toCache(event.tag, event.data)
    .catch( reportError(event) )
})


function removeCache(key) {
  return deleteFromIndex({ key: key })
}


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



// Cache (Tracks)
// --------------


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


app.ports.storeTracksInCache.subscribe(list => {
  list.reduce(
    (acc, item) => { return acc
      .then(_ => fetch(item.url))
      .then(r => r.blob())
      .then(b => setInIndex({ key: item.trackId, data: b, store: storeNames.tracks }))
    },
    Promise.resolve()

  ).then(
    _ => self.postMessage({
      tag: "STORE_TRACKS_IN_CACHE",
      data: list.map(l => l.trackId),
      error: null
    })

  ).catch(
    err => {
      self.postMessage({
        tag: "STORE_TRACKS_IN_CACHE",
        data: list.map(l => l.trackId),
        error: err.message || err
      })
    }

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



// 🛠


function reportError(event) {
  return err => {
    console.error(err.message, err.stack)
    app.ports.fromAlien.send({ tag: event.tag, data: null, error: err.message || err })
  }
}
