import type { App } from "./elm/types"
import { db } from "../common"
import { reportError } from "./common"
import { transformUrl } from "../urls"


// 🏔️


let app: App



// 🚀


export function init(a: App) {
  app = a

  app.ports.downloadTracks.subscribe(downloadTracks)
  app.ports.removeTracksFromCache.subscribe(removeTracksFromCache)
  app.ports.storeTracksInCache.subscribe(storeTracksInCache)
}



// PORTS


function downloadTracks(group) {
  self.postMessage({
    action: "DOWNLOAD_TRACKS",
    data: group
  })
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
