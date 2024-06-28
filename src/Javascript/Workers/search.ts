//
// Search worker
// (◡ ‿ ◡ ✿)
//
// This worker is responsible for searching through a `Track` collection.


import lunr from "lunr"


const FIELDS = ["album", "artist", "title"]


lunr.Pipeline.registerFunction(
  removeParenthesesFromToken,
  "Remove parentheses from token"
)


let index: lunr.Index



// Incoming messages
// -----------------

self.onmessage = (event: MessageEvent) => {
  switch (event.data.action) {
    case "PERFORM_SEARCH":
      performSearch(event.data.data)
      break

    case "UPDATE_SEARCH_INDEX":
      updateSearchIndex(event.data.data)
      break
  }
}



// Mapper
// ------

const mapTrack = track => ({
  id: track.id,
  album: track.tags.album,
  artist: track.tags.artist,
  title: track.tags.title,
})



// Actions
// -------

function performSearch(rawSearchTerm: string) {
  let results: string[] =
    []

  const searchTerm = rawSearchTerm
    .replace(/-\s+/g, "-")
    .replace(/\+\s+/g, "+")
    .split(/ +/)
    .reduce(
      ([ acc, previousOperator, previousPrefix ]: [ string[], string, string ], chunk: string): [ string[], string, string ] => {
        const operator = (a => a && a[0])( chunk.match(/^(\+|-)/) )

        let chunkWithoutOperator = chunk.replace(/^(\+|-)/, "").replace(/\*$/, "").trim()
        let prefix = (a => a && a[1])( chunkWithoutOperator.match(/^([^:]+:)/) )
        let chunkWithoutPrefix = chunkWithoutOperator.replace(/^([^:]+:)/, "")

        if (prefix && !FIELDS.includes(prefix.slice(0, -1))) {
          prefix = null
          chunkWithoutPrefix = chunkWithoutOperator.replace(":", "\\:")
          chunkWithoutOperator = chunkWithoutPrefix

        } else if (prefix && chunkWithoutPrefix.includes(":")) {
          chunkWithoutPrefix = chunkWithoutPrefix.replace(":", "\\:")
          chunkWithoutOperator = prefix + chunkWithoutPrefix

        }

        const op = operator || previousOperator
        const pr = prefix ? "" : (operator ? "" : previousPrefix)

        return chunkWithoutPrefix.trim().length > 0
          ? [ [ ...acc
              , op + pr + chunkWithoutOperator
              ]
              , op
              , prefix || pr
              ]
          : [ acc, previousOperator, previousPrefix ]
      },
      [ [], "+", "" ]
    )[0]
    .join(" ")

  const searchTermWithAsteriks =
    searchTerm
      .split(" ")
      .map(s => {
        if (s.startsWith("-")) return s
        return s + "*"
      })
      .join(" ")

  if (index) {
    results = index
      .search(searchTerm)
      .map(s => s.ref)
      .concat(
        index
          .search(searchTermWithAsteriks)
          .map(s => s.ref)
      )
  }

  self.postMessage({
    action: "PERFORM_SEARCH",
    data: results
  })
}


function updateSearchIndex(input: string | object[]) {
  const tracks = (typeof input == "string")
    ? JSON.parse(input)
    : input

  index = customLunr((builder: lunr.Builder) => {
    FIELDS.forEach(
      field => builder.field(field)
    )

    ;(tracks || [])
      .map(mapTrack)
      .forEach(t => builder.add(t))
  })
}



function customLunr(fn: (b: lunr.Builder) => void) {
  const builder = new lunr.Builder

  builder.pipeline.add(removeParenthesesFromToken, lunr.stemmer)
  builder.searchPipeline.add(removeParenthesesFromToken, lunr.stemmer)

  fn(builder)
  return builder.build()
}


function removeParenthesesFromToken(token: lunr.Token): lunr.Token {
  return token.update(s => s.replace(/\(|\)/, ""))
}
