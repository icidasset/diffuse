//
// Album Covers
// (◕‿◕✿)

import * as Uint8arrays from "uint8arrays"

import * as processing from "./processing"
import { type App } from "./elm/types"
import { transformUrl } from "../urls"
import { toCache } from "./common"
import { type CoverPrep } from "../common"


// 🌳


type CoverPrepWithUrls = CoverPrep & {
  trackGetUrl: string
  trackHeadUrl: string
}



// 🏔️


let artworkQueue: CoverPrep[] = []
let app: App



// 🚀


export function init(a: App) {
  app = a

  app.ports.provideArtworkTrackUrls.subscribe(provideArtworkTrackUrls)
}



// PORTS


function provideArtworkTrackUrls(prep: CoverPrepWithUrls) {
  find(prep).then(blob => {
    return toCache(`coverCache.${prep.cacheKey}`, blob).then(_ => blob)
  })
  .then((blob: Blob) => {
    const url = URL.createObjectURL(blob)

    self.postMessage({
      tag: "GOT_CACHED_COVER",
      data: { imageType: blob.type, key: prep.cacheKey, url: url },
      error: null
    })
  })
  .catch(err => {
    if (err === "No artwork found") {
      // Indicate that we've tried to find artwork,
      // so that we don't try to find it each time we launch the app.
      return toCache(`coverCache.${prep.cacheKey}`, "TRIED")

    } else {
      // Something went wrong
      console.error(err)
      return toCache(`coverCache.${prep.cacheKey}`, "TRIED")

    }
  })
  .catch(() => {
    console.warn("Failed to download artwork for ", prep)
  })
  .finally(shiftQueue)
}



// 🛠️


export function download(list: CoverPrep[]) {
  const exe = !artworkQueue[0]
  artworkQueue = artworkQueue.concat(list)
  if (exe) shiftQueue()
}


function shiftQueue() {
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



// ㊙️


const REJECT = () => Promise.reject("No artwork found")


function decodeCacheKey(cacheKey: string) {
  return Uint8arrays.toString(
    Uint8arrays.fromString(cacheKey, "base64"),
    "utf8"
  )
}


function find(prep: CoverPrepWithUrls) {
  return findUsingTags(prep)
    .then(a => a ? a : findUsingMusicBrainz(prep))
    .then(a => a ? a : findUsingLastFm(prep))
    .then(a => a ? a : REJECT())
    .then(a => a.type.startsWith("image/") ? a : REJECT())
}



// 1. TAGS


async function findUsingTags(prep: CoverPrepWithUrls) {
  return Promise.all(
    [
      transformUrl(prep.trackHeadUrl, app),
      transformUrl(prep.trackGetUrl, app)
    ]

  ).then(([ headUrl, getUrl ]) => processing.getTags(
    headUrl,
    getUrl,
    prep.trackFilename,
    { covers: true }

  )).then(tags => {
    return tags?.picture
      ? new Blob([ tags.picture.data ], { type: tags.picture.format })
      : null

  })
}



// 2. MUSIC BRAINZ


function findUsingMusicBrainz(prep: CoverPrepWithUrls) {
  if (!navigator.onLine) return null

  const parts = decodeCacheKey(prep.cacheKey).split(" --- ")
  const artist = parts[ 0 ]
  const album = parts[ 1 ] || parts[ 0 ]

  const query = `release:"${album}"` + (prep.variousArtists === "t" ? `` : ` AND artist:"${artist}"`)
  const encodedQuery = encodeURIComponent(query)

  return fetch(`https://musicbrainz.org/ws/2/release/?query=${encodedQuery}&fmt=json`)
    .then(r => r.json())
    .then(r => musicBrainzCover(r.releases))
}


function musicBrainzCover(remainingReleases) {
  const release = remainingReleases[ 0 ]
  if (!release) return null

  return fetch(
    `https://coverartarchive.org/release/${release.id}/front-500`
  ).then(
    r => r.blob()
  ).then(
    r => r && r.type.startsWith("image/")
      ? r
      : musicBrainzCover(remainingReleases.slice(1))
  ).catch(
    () => musicBrainzCover(remainingReleases.slice(1))
  )
}



// 3. LAST FM


function findUsingLastFm(prep: CoverPrepWithUrls) {
  if (!navigator.onLine) return null

  const query = encodeURIComponent(
    decodeCacheKey(prep.cacheKey).replace(" --- ", " ")
  )

  return fetch(`https://ws.audioscrobbler.com/2.0/?method=album.search&album=${query}&api_key=4f0fe85b67baef8bb7d008a8754a95e5&format=json`)
    .then(r => r.json())
    .then(r => lastFmCover(r.results.albummatches.album))
}


function lastFmCover(remainingMatches) {
  const album = remainingMatches[ 0 ]
  const url = album ? album.image[ album.image.length - 1 ][ "#text" ] : null

  return url && url !== ""
    ? fetch(url)
      .then(r => r.blob())
      .catch(_ => lastFmCover(remainingMatches.slice(1)))
    : album && lastFmCover(remainingMatches.slice(1))
}
