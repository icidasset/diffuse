//
// Search worker
// (◡ ‿ ◡ ✿)
//
// This worker is responsible for searching through a `Track` collection.


import lunr from "lunr"


lunr.Pipeline.registerFunction(
  removeParenthesesFromToken,
  "Remove parentheses from token"
)


let index



// Incoming messages
// -----------------

self.onmessage = event => {
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

function performSearch(rawSearchTerm) {
  let results =
    []

  const searchTerm = rawSearchTerm
    .replace(/-\s+/g, "-")
    .replace(/\+\s+/g, "+")
    .split(/ +/)
    .reduce(
      ([ acc, previousOperator, previousPrefix ], chunk) => {
        const operator = (a => a && a[0])( chunk.match(/^(\+|-)/) )
        const chunkWithoutOperator = chunk.replace(/^(\+|-)/, "")
        const prefix = (a => a && a[1])( chunkWithoutOperator.match(/^([^:]+:)/) )
        const chunkWithoutPrefix = chunkWithoutOperator.replace(/^([^:]+:)/, "")

        const op = operator || previousOperator
        const pr = prefix ? "" : (operator ? "" : previousPrefix)

        return chunkWithoutPrefix.trim().length > 0
          ? [ [ ...acc, op + pr + chunkWithoutOperator ], op, prefix || pr ]
          : [ acc, previousOperator, previousPrefix ]
      },
      [ [], "+", "" ]
    )[0]
    .join(" ")

  if (index) {
    results = index
      .search(searchTerm)
      .map(s => s.ref)
  }

  self.postMessage({
    action: "PERFORM_SEARCH",
    data: results
  })
}


function updateSearchIndex(input) {
  const tracks = (typeof input == "string")
    ? JSON.parse(input)
    : input

  index = customLunr(function() {
    this.field("album");
    this.field("artist");
    this.field("title");

    (tracks || [])
      .map(mapTrack)
      .forEach(t => this.add(t))
  })
}



function customLunr(config) {
  const builder = new lunr.Builder

  builder.pipeline.add(removeParenthesesFromToken, lunr.stemmer)
  builder.searchPipeline.add(removeParenthesesFromToken, lunr.stemmer)

  config.call(builder, builder)
  return builder.build()
}


function removeParenthesesFromToken(token) {
  return token.update(s => s.replace(/\(|\)/, ""))
}
