//
// Brain
// 🧠
//
// This worker is responsible for everything non-UI.

import * as Application from "./application"
import * as Artwork from "./artwork"
import * as Processing from "./processing"
import * as User from "./user"
import * as TaskPorts from "./task-ports"
import * as UI from "./ui"

import { db } from "../common"
import { fromCache, removeCache, reportError } from "./common"
import { sendData, toCache } from "./common"
import { transformUrl } from "../urls"


// 🚀


TaskPorts.register()
User.TaskPorts.register()

const app = Application.load()
const brain = self as unknown as Worker

// 🖼️
UI.link(brain, app)

// ⚡
User.Ports.register(app)

// wire.ui()
// wire.caching()
// wire.artworkCaching()
// wire.tracksCaching()
// wire.downloading()
// wire.search()
// wire.tags()

// 🛫
brain.postMessage({ action: "READY" })



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
  const exe = !artworkQueue[0]
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
  Artwork
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
    .catch(() => {
      console.warn("Failed to download artwork for ", prep)
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
    (acc, id) => acc.then(_ => db("tracks").removeItem(id)),
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
        .then(_ => transformUrl(item.url, app))
        .then(fetch)
        .then(r => r.blob())
        .then(b => db("tracks").setItem(item.trackId, b))
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

const search = new Worker(
  "../../search.js",
  { type: "module" }
)


wire.search = () => {
  app.ports.requestSearch.subscribe(requestSearch)
  app.ports.updateSearchIndex.subscribe(updateSearchIndex)
}


function requestSearch(searchTerm: string) {
  search.postMessage({
    action: "PERFORM_SEARCH",
    data: searchTerm
  })
}


function updateSearchIndex(tracksJson: string) {
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
    Processing.processContext(context, app).then(newContext => {
      app.ports.receiveTags.send(newContext)
    })
  })

  app.ports.syncTags.subscribe(context => {
    Processing.processContext(context, app).then(newContext => {
      app.ports.replaceTags.send(newContext)
    })
  })
}
