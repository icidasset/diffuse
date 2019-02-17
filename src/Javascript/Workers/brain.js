//
// Brain
// 🧠
//
// This worker is responsible for everything non-UI.

importScripts("/vendor/music-metadata.js")

importScripts("/brain.js")
importScripts("/indexed-db.js")
importScripts("/processing.js")
importScripts("/urls.js")


const app = Elm.Brain.init()



// UI
// ==

self.onmessage = event => {
  app.ports.fromUI.send(event.data)
}

app.ports.toUI.subscribe(event => {
  self.postMessage(event)
})



// Cache
// -----

app.ports.removeCache.subscribe(event => {
  deleteFromIndex({ key: event.tag }).catch(console.error)
})


app.ports.requestCache.subscribe(event => {
  getFromIndex({ key: event.tag }).then(data => {
    app.ports.fromCache.send({
      tag: event.tag,
      data: data,
      error: null
    })
  }).catch(
     console.error
  )
})


app.ports.toCache.subscribe(event => {
  setInIndex({ key: event.tag, data: event.data }).catch(console.error)
})



// Search
// ------

const search = new Worker("/workers/search.js")


app.ports.requestSearch.subscribe(searchTerm => {
  search.postMessage({
    action: "PERFORM_SEARCH",
    data: searchTerm
  })
})


app.ports.updateSearchIndex.subscribe(tracksJson => {
  search.postMessage({
    action: "UPDATE_SEARCH_INDEX",
    data: tracksJson
  })
})


search.onmessage = event => {
  switch (event.data.action) {
    case "PERFORM_SEARCH":
      app.ports.receiveSearchResults.send(event.data.data)
      break
  }
}



// Tags
// ----

app.ports.requestTags.subscribe(context => {
  processContext(context).then(newContext => {
    app.ports.receiveTags.send(newContext)
  })
})
