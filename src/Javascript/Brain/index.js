//
// Brain
// 🧠
//
// This worker is responsible for everything non-UI.

import "subworkers"
import { StreamingHttpTokenReader } from "streaming-http-token-reader"

import * as crypto from "../crypto.js"
import * as db from "../indexed-db.js"
import * as processing from "../processing.js"
import * as urls from "../urls.js"
import * as user from "./user.js"

import { fromCache, removeCache, reportError } from "./common.js"
import { sendData, storageCallback, toCache } from "./common.js"

importScripts("brain.elm.js")


const flags = location
  .hash
  .substr(1)
  .split("&")
  .reduce((acc, flag) => {
    const [k, v] = flag.split("=")
    return { ...acc, [k]: v }
  }, {})


const app = Elm.Brain.init({
  flags: {
    initialUrl: decodeURIComponent(flags.appHref) || ""
  }
})


user.setupPorts(app)



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
  // Nothing here yet
}}



// Cache
// -----

app.ports.removeCache.subscribe(event => {
  removeCache(event.tag)
    .catch( reportError(app, event) )
})


app.ports.requestCache.subscribe(event => {
  const key = event.data && event.data.file
    ? event.tag + "_" + event.data.file
    : event.tag

  fromCache(key)
    .then( sendData(app, event) )
    .catch( reportError(app, event) )
})


app.ports.toCache.subscribe(event => {
  const key = event.data && event.data.file
    ? event.tag + "_" + event.data.file
    : event.tag

  toCache(key, event.data.data || event.data)
    .then( storageCallback(app, event) )
    .catch( reportError(app, event) )
})



// Cache (Tracks)
// --------------

app.ports.removeTracksFromCache.subscribe(trackIds => {
  trackIds.reduce(
    (acc, id) => acc.then(_ => db.deleteFromIndex({ key: id, store: db.storeNames.tracks })),
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
      .then(b => db.setInIndex({ key: item.trackId, data: b, store: db.storeNames.tracks }))
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
      console.error(err)
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
  processing.processContext(context).then(newContext => {
    app.ports.receiveTags.send(newContext)
  })
})
