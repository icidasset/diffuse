//
// Brain
// 🧠
//
// This worker is responsible for everything non-UI.


import * as TaskPort from "elm-taskport"

import * as artwork from "./artwork"
import * as db from "../indexed-db"
import * as processing from "../processing"
import * as user from "./user"

import { fromCache, removeCache, reportError } from "./common"
import { sendData, toCache } from "./common"
import { transformUrl } from "../urls"

importScripts("brain.elm.js")
importScripts("subworkers.js")


// 🍱


let app
let wire = {}


TaskPort.install()


TaskPort.register("fromCache", fromCache)
TaskPort.register("removeCache", removeCache)
TaskPort.register("toCache", ({ key, value }) => toCache(key, value))


user.setupTaskPorts()


const flags = location
  .hash
  .substr(1)
  .split("&")
  .reduce((acc, flag) => {
    const [ k, v ] = flag.split("=")
    return { ...acc, [ k ]: v }
  }, {})


forwardCompatibility().then(initialise)


function initialise() {
  app = Elm.Brain.init({
    flags: {
      initialUrl: decodeURIComponent(flags.appHref) || ""
    }
  })

  user.setupPorts(app)

  wire.ui()
  wire.caching()
  wire.artworkCaching()
  wire.tracksCaching()
  wire.downloading()
  wire.search()
  wire.tags()
}


async function forwardCompatibility() {
  // TODO: Future, check version to migrate
  if (await fromCache("MIGRATED")) return

  await moveOldDbValue({ oldName: "AUTH_SECRET_KEY", newName: "SECRET_KEY" })
  await moveOldDbValue({ oldName: "AUTH_ENCLOSED_DATA", newName: "ENCLOSED_DATA" })

  const method = await fromCache("AUTH_METHOD")

  if (method === "LOCAL") {
    await moveOldDbValue({ oldName: "AUTH_ANONYMOUS_favourites.json", newName: "SYNC_LOCAL_favourites.json", parseJSON: true })
    await moveOldDbValue({ oldName: "AUTH_ANONYMOUS_playlists.json", newName: "SYNC_LOCAL_playlists.json", parseJSON: true })
    await moveOldDbValue({ oldName: "AUTH_ANONYMOUS_progress.json", newName: "SYNC_LOCAL_progress.json", parseJSON: true })
    await moveOldDbValue({ oldName: "AUTH_ANONYMOUS_settings.json", newName: "SYNC_LOCAL_settings.json", parseJSON: true })
    await moveOldDbValue({ oldName: "AUTH_ANONYMOUS_sources.json", newName: "SYNC_LOCAL_sources.json", parseJSON: true })
    await moveOldDbValue({ oldName: "AUTH_ANONYMOUS_tracks.json", newName: "SYNC_LOCAL_tracks.json", parseJSON: true })

    await removeCache("AUTH_METHOD")

  } else if (method) {
    await toCache("SYNC_METHOD", method)
    await removeCache("AUTH_METHOD")

  }

  await toCache("MIGRATED", "3.3.0")
}


async function moveOldDbValue({ oldName, newName, parseJSON }) {
  const value = await fromCache(oldName)
  if (value) {
    await toCache(newName, parseJSON ? JSON.parse(value) : value)
    await removeCache(oldName)
  }
}



// UI
// ==

wire.ui = () => {
  app.ports.toUI.subscribe(event => {
    self.postMessage(event)
  })


  self.onmessage = event => {
    if (event.data.action) return handleAction(event.data.action, event.data.data)
    if (event.data.tag) return app.ports.fromAlien.send(event.data)
  }
}


function handleAction(action, data) {
  switch (action) {
    case "DOWNLOAD_ARTWORK": return downloadArtwork(data)
  }
}



// Cache
// -----

wire.caching = () => {
  app.ports.removeCache.subscribe(event => {
    removeCache(event.tag)
      .catch(reportError(app, event))
  })

  app.ports.requestCache.subscribe(event => {
    const key = event.data && event.data.file
      ? event.tag + "_" + event.data.file
      : event.tag

    fromCache(key)
      .then(sendData(app, event))
      .catch(reportError(app, event))
  })

  app.ports.toCache.subscribe(event => {
    const key = event.data && event.data.file
      ? event.tag + "_" + event.data.file
      : event.tag

    toCache(key, event.data.data || event.data)
      .catch(reportError(app, event))
  })
}



// Cache (Artwork)
// ---------------

let artworkQueue = []


wire.artworkCaching = () => {
  app.ports.provideArtworkTrackUrls.subscribe(provideArtworkTrackUrls)
}


function downloadArtwork(list) {
  const exe = !artworkQueue[ 0 ]
  artworkQueue = artworkQueue.concat(list)
  if (exe) shiftArtworkQueue()
}


function shiftArtworkQueue() {
  const next = artworkQueue.shift()

  if (next) {
    app.ports.makeArtworkTrackUrls.send(next)
  } else {
    self.postMessage({
      action: "FINISHED_DOWNLOADING_ARTWORK",
      data: null
    })
  }
}


function provideArtworkTrackUrls(prep) {
  artwork
    .find(prep, app)
    .then(blob => {
      const url = URL.createObjectURL(blob)

      self.postMessage({
        tag: "GOT_CACHED_COVER",
        data: { key: prep.cacheKey, url: url },
        error: null
      })

      return toCache(`coverCache.${prep.cacheKey}`, blob)
    })
    .catch(err => {
      if (err === "No artwork found") {
        // Indicate that we've tried to find artwork,
        // so that we don't try to find it each time we launch the app.
        return toCache(`coverCache.${prep.cacheKey}`, "TRIED")

      } else {
        // Something went wrong
        reportError(app, { tag: "REPORT_ERROR" })(err)

      }
    })
    .finally(shiftArtworkQueue)
}



// Cache (Tracks)
// --------------

wire.tracksCaching = () => {
  app.ports.removeTracksFromCache.subscribe(removeTracksFromCache)
  app.ports.storeTracksInCache.subscribe(storeTracksInCache)
}


function removeTracksFromCache(trackIds) {
  trackIds.reduce(
    (acc, id) => acc.then(_ => db.deleteFromIndex({ key: id, store: db.storeNames.tracks })),
    Promise.resolve()

  ).catch(
    _ => reportError
      (app, { tag: "REMOVE_TRACKS_FROM_CACHE" })
      ("Failed to remove tracks from cache")

  )
}


function storeTracksInCache(list) {
  list.reduce(
    (acc, item) => {
      return acc
        .then(_ => transformUrl(item.url))
        .then(fetch)
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
}



// Downloading
// -----------

wire.downloading = () => {
  app.ports.downloadTracks.subscribe(group => {
    self.postMessage({
      action: "DOWNLOAD_TRACKS",
      data: group
    })
  })
}



// Search
// ------

const search = new Worker("search.js")


wire.search = () => {
  app.ports.requestSearch.subscribe(requestSearch)
  app.ports.updateSearchIndex.subscribe(updateSearchIndex)
}


function requestSearch(searchTerm) {
  search.postMessage({
    action: "PERFORM_SEARCH",
    data: searchTerm
  })
}


function updateSearchIndex(tracksJson) {
  search.postMessage({
    action: "UPDATE_SEARCH_INDEX",
    data: tracksJson
  })
}


search.onmessage = event => {
  switch (event.data.action) {
    case "PERFORM_SEARCH":
      app.ports.receiveSearchResults.send(event.data.data)
      break
  }
}



// Tags
// ----

wire.tags = () => {
  app.ports.requestTags.subscribe(context => {
    processing.processContext(context, app).then(newContext => {
      app.ports.receiveTags.send(newContext)
    })
  })

  app.ports.syncTags.subscribe(context => {
    processing.processContext(context, app).then(newContext => {
      app.ports.replaceTags.send(newContext)
    })
  })
}
