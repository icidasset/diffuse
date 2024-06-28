import type { App } from "./elm/types"


// 🏔️


let app: App



// 🚀


export function init(a: App) {
  app = a

  app.ports.requestSearch.subscribe(requestSearch)
  app.ports.updateSearchIndex.subscribe(updateSearchIndex)
}


const search = new Worker(
  "../../search.js",
  { type: "module" }
)


search.onmessage = event => {
  switch (event.data.action) {
    case "PERFORM_SEARCH":
      app.ports.receiveSearchResults.send(event.data.data)
      break
  }
}



// PORTS


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
