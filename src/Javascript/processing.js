//
// Processing
// ♪(´ε｀ )
//
// Audio processing, getting metadata, etc.


import * as musicMetadata from "music-metadata-browser"
import { makeTokenizer } from "@tokenizer/http"

import { mimeType } from "./common"
import { transformUrl } from "./urls"


// Contexts
// --------

export function processContext(context) {
  const initialPromise = Promise.resolve([])

  return context.urlsForTags.reduce((accumulator, urls, idx) => {
    return accumulator.then(col => {
      const filename = context
        .receivedFilePaths[idx]
        .split("/")
        .reverse()[0]

      return Promise.all([
        transformUrl(urls.headUrl),
        transformUrl(urls.getUrl)

      ]).then(([headUrl, getUrl]) => {
        return getTags(headUrl, getUrl, filename)

      }).then(r => {
        return col.concat(r)

      }).catch(e => {
        console.error(e)
        return col.concat(null)

      })
    })

  }, initialPromise).then(col => {
    context.receivedTags = col
    return context

  })
}



// Tags
// ----


const parserConfiguration = Object.assign(
  {}, musicMetadata.parsingOptions,
  { duration: false, skipCovers: true, skipPostHeaders: true }
)


function getTags(headUrl, getUrl, filename) {
  const fileExtMatch = filename.match(/\.(\w+)$/)
  const fileExt = fileExtMatch && fileExtMatch[1]

  const overrideContentType = (
    getUrl.includes("googleapis.com") ||
    getUrl.includes("googleusercontent.com")
  )

  return makeTokenizer(headUrl)
    .then(tokenizer => {
      const fileMime = overrideContentType
        ? mimeType(fileExt)
        : tokenizer.fileInfo.mimeType

      tokenizer.fileInfo.mimeType = fileMime
      tokenizer.fileInfo.url = getUrl
      tokenizer.rangeRequestClient.url = getUrl
      tokenizer.rangeRequestClient.resolvedUrl = undefined

      const originalGetResponse = tokenizer.rangeRequestClient.getResponse

      tokenizer.rangeRequestClient.getResponse =
        function() {
          this.resolvedUrl = getUrl
          return originalGetResponse.apply(this, arguments)
        }

      return musicMetadata.parseFromTokenizer(
        tokenizer,
        parserConfiguration
      )
    })
    .then(pickTags)
    .catch(err => {
      console.error(err)
      return fallbackTags(filename)
    })
}


function pickTags(result) {
  const tags = result && result.common
  if (!tags) return null

  return {
    disc: tags.disk.no || 1,
    nr: tags.track.no || 1,
    album: tags.album && tags.album.length ? tags.album : "Unknown",
    artist: tags.artist && tags.artist.length ? tags.artist : "Unknown",
    title: tags.title && tags.title.length ? tags.title : "Unknown",
    genre: (tags.genre && tags.genre[0]) || null,
    year: tags.year || null,
    picture: null
  }
}


function fallbackTags(filename) {
  const filenameWithoutExt = filename.replace(/\.\w+$/, "")

  return {
    disc: 1,
    nr: 1,
    album: "Unknown",
    artist: "Unknown",
    title: filenameWithoutExt,
    genre: null,
    year: null,
    picture: null
  }
}
