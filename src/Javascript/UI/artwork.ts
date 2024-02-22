import { debounce } from "throttle-debounce"

import type { App } from "./elm/types"
import { db } from "../common"


// 🌳


type CoverPrep = {
  cacheKey: string
  trackFilename: string
  trackPath: string
  trackSourceId: string
  variousArtists: string
}



// 🏔️


let app: App
let brain: Worker



// 🚀


export function init(a: App, b: Worker) {
  app = a
  brain = b

  app.ports.loadAlbumCovers.subscribe(
    debounce(500, loadAlbumCoversFromDom)
  )

  db().keys().then(cachedCovers)
}



// 🛠️


function albumCover(coverKey: string): Promise<Blob | null> {
  return db().getItem(`coverCache.${coverKey}`)
}


function gotCachedCover({ key, url }) {
  const item = orchestrion.activeQueueItem

  if (item && orchestrion.coverPrep && key === orchestrion.coverPrep.key && url) {
    let artwork = [{ src: url, type: undefined }]

    if (typeof url !== "string") {
      artwork = [{
        src: URL.createObjectURL(url),
        type: url.type
      }]
    }

    navigator.mediaSession.metadata = new MediaMetadata({
      title: item.trackTags.title,
      artist: item.trackTags.artist,
      album: item.trackTags.album,
      artwork: artwork
    })
  }
}


async function loadAlbumCoversFromDom({ coverView, list }: { coverView: boolean, list: boolean }): Promise<void> {
  let nodes: HTMLElement[] = []

  if (list) nodes = nodes.concat(Array.from(
    document.querySelectorAll("#diffuse__track-covers [data-key]")
  ))

  if (coverView) nodes = nodes.concat(Array.from(
    document.querySelectorAll("#diffuse__track-covers + div [data-key]")
  ))

  if (!nodes.length) return;

  const coverPrepList = nodes.reduce((acc: CoverPrep[], node: HTMLElement) => {
    const a = {
      cacheKey: node.getAttribute("data-key"),
      trackFilename: node.getAttribute("data-filename"),
      trackPath: node.getAttribute("data-path"),
      trackSourceId: node.getAttribute("data-source-id"),
      variousArtists: node.getAttribute("data-various-artists")
    }

    if (a.cacheKey && a.trackFilename && a.trackPath && a.trackSourceId && a.variousArtists) {
      return [...acc, a as CoverPrep]
    } else {
      return acc
    }
  }, [] as CoverPrep[])

  return loadAlbumCovers(coverPrepList)
}


async function loadAlbumCovers(coverPrepList: CoverPrep[]): Promise<void> {
  const withoutEarlierAttempts = await coverPrepList.reduce(async (
    acc: Promise<CoverPrep[]>,
    prep: CoverPrep
  ): Promise<CoverPrep[]> => {
    const arr = await acc
    const a = await albumCover(prep.cacheKey)
    if (!a) return [...arr, prep]
    return arr
  }, Promise.resolve([]))

  brain.postMessage({
    action: "DOWNLOAD_ARTWORK",
    data: withoutEarlierAttempts
  })
}


// Send a dictionary of the cached covers to the app.
function cachedCovers(keys: string[]) {
  const cacheKeys = keys.filter(
    k => k.startsWith("coverCache.")
  )

  const cachePromise = cacheKeys.reduce((acc, key) => {
    return acc.then(cache => {
      return db().getItem(key).then(blob => {
        const cacheKey = key.slice(11)

        if (blob && typeof blob !== "string" && blob instanceof Blob) {
          cache[cacheKey] = URL.createObjectURL(blob)
        }

        return cache
      })
    })
  }, Promise.resolve({}))

  cachePromise.then(cache => {
    app.ports.insertCoverCache.send(cache)
    setTimeout(() => loadAlbumCoversFromDom({ list: true, coverView: true }), 500)
  })
}


function finishedDownloadingArtwork() {
  if (!orchestrion.audio || !orchestrion.audio.waitingForArtwork || !orchestrion.activeQueueItem) return

  albumCover(orchestrion.audio.waitingForArtwork).then(maybeArtwork => {
    audioEngine.setMediaSessionMetadata(orchestrion.activeQueueItem, maybeArtwork)
  })

  orchestrion.audio.waitingForArtwork = null
}
