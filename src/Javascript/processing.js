//
// Processing
// ♪(´ε｀ )
//
// Audio processing, getting metadata, etc.


// Contexts
// --------

function processContext(context) {
  const initialPromise = Promise.resolve([])

  return context.urlsForTags.reduce((accumulator, urls, idx) => {
    let getUrl
    let headUrl

    return accumulator.then(col => {
      const filename = context
        .receivedFilePaths[idx]
        .split("/")
        .reverse()[0]
        .replace(/\.\w+$/, "")

      return transformUrl(urls.getUrl)
        .then(url => { getUrl = url; return transformUrl(urls.headUrl) })
        .then(url => { headUrl = url; return getTags(getUrl, headUrl, filename) })
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


const readerConfiguration = {
  timeoutInSec: 300,
  avoidHeadRequests: false
}


const parserConfiguration = Object.assign(
  {}, musicMetadata.parsingOptions,
  { duration: false, skipCovers: true, skipPostHeaders: true }
)



function getTags(getUrl, headUrl, filename) {
  const reader = new StreamingHttpTokenReader(headUrl, readerConfiguration)

  return reader.init().then(_ => {
    reader.url = getUrl

    return musicMetadata.parseFromTokenizer(
      reader,
      reader.contentType,
      parserConfiguration
    )
  })
  .then(pickTags)
  .catch(_ => fallbackTags(filename))
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
  return {
    disc: 1,
    nr: 1,
    album: "Unknown",
    artist: "Unknown",
    title: filename,
    genre: null,
    year: null,
    picture: null
  }
}
