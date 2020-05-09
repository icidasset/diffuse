//
// Album Covers
// (◕‿◕✿)


import * as db from "../indexed-db"
import * as processing from "../processing"

import { toCache } from "./common"


const REJECT = () => Promise.reject("No artwork found")


export function find(prep) {
  return findUsingTags(prep)
    .then(a => a ? a : findUsingMusicBrainz(prep))
    .then(a => a ? a : findUsingLastFm(prep))
    .then(a => a ? a : REJECT())
    .then(a => a.type.startsWith("image/") ? a : REJECT())
}



// 1. TAGS


function findUsingTags(prep) {
  return processing.getTags(
    prep.trackHeadUrl,
    prep.trackGetUrl,
    prep.trackFilename,
    { skipCovers: false }

  ).then(tags => {
    return tags.picture
      ? new Blob([ tags.picture.data ], { type: tags.picture.format })
      : null

  })
}



// 2. MUSIC BRAINZ


function findUsingMusicBrainz(prep) {
  const parts = atob(prep.cacheKey).split(" --- ")
  const artist = parts[0]
  const album = parts[1]

  const query = `release:"${album}" AND artist:"${artist}"`
  const encodedQuery = encodeURIComponent(query)

  return fetch(`https://musicbrainz.org/ws/2/release/?query=${encodedQuery}&fmt=json`)
    .then(r => r.json())
    .then(r => {
      const release = r.releases[0]
      if (!release) return null

      return fetch(
        `https://coverartarchive.org/release/${release.id}/front-500`
      ).then(
        r => r.blob()
      )
    })
}



// 3. LAST FM


function findUsingLastFm(prep) {
  const query = atob(prep.cacheKey).replace(" --- ", " ")

  return fetch(`https://ws.audioscrobbler.com/2.0/?method=album.search&album=${query}&api_key=4f0fe85b67baef8bb7d008a8754a95e5&format=json`)
    .then(r => r.json())
    .then(r => {
      const album = r.results.albummatches.album[0]
      const url = album ? album.image[album.image.length - 1]["#text"] : null

      return url && url !== ""
        ? fetch(url).then(r => r.blob())
        : null
    })
}
