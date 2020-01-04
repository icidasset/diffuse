//
// Processing
// ♪(´ε｀ )
//
// Audio processing, getting metadata, etc.


import * as musicMetadata from "music-metadata-browser"

import { HttpClient } from "@tokenizer/http/lib/http-client"
import { parseContentRange } from "@tokenizer/range"
import { RangeRequestFactory } from "@tokenizer/range/lib/range-request-factory"
import { RangeRequestTokenizer } from "@tokenizer/range/lib/range-request-tokenizer"

import { mimeType } from "./common"
import { transformUrl } from "./urls"


// Contexts
// --------

export function processContext(context) {
  const initialPromise = Promise.resolve([])

  return context.urlsForTags.reduce((accumulator, urls, idx) => {
    return accumulator.then(col => {
      let get, head

      const filename = context
        .receivedFilePaths[idx]
        .split("/")
        .reverse()[0]

      return transformUrl(urls.headUrl)
        .then(url => resolveUrl("HEAD", url))
        .then(res => head = res)

        .then(_ => (urls.headUrl === urls.getUrl) && head.mime && head.size
          ? head
          : transformUrl(urls.getUrl).then(url => resolveUrl("GET", url))
        )
        .then(res => get = res)

        .then(_ => getTags(head, get, filename))
        .then(r => col.concat(r))

        .catch(e => {
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


function getTags(head, get, filename) {
  const fileExtMatch = filename.match(/\.(\w+)$/)
  const fileExt = fileExtMatch && fileExtMatch[1]

  // Content type
  const overrideContentType = (
    get.url.includes("googleapis.com") ||
    get.url.includes("googleusercontent.com")
  )

  const fileMime = overrideContentType
    ? mimeType(fileExt)
    : get.mime

  // Tokenizer
  const tokenizer = new RangeRequestTokenizer(
    new HttpClient(get.url),
    Object.assign({}, get, { mimeType: fileMime }),
    1024
  )

  // Get tags
  return musicMetadata.parseFromTokenizer(
    tokenizer,
    parserConfiguration
  )
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


function resolveUrl(method, url) {
  return fetch(url, {
    method: method,
    headers: method === "HEAD"
      ? new Headers()
      : new Headers({ "Range": "bytes=0-0" })

  }).then(resp => {
    const length = resp.headers.get("content-length")
    const rangeString = resp.headers.get("content-range")
    const range = parseContentRange(rangeString)

    return {
      contentRange: range,
      mimeType: resp.headers.get("content-type"),
      size: range ? range.instanceLength : parseInt(length, 10),
      url: resp.url
    }
  })
}
