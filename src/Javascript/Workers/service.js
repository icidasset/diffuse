//
// Service worker
// (◡ ‿ ◡ ✿)
//
// This worker is responsible for caching the application
// so it can be used offline.

importScripts("version.js")


const KEY =
  "diffuse-" + self.VERSION


const exclude =
  [ "_headers"
  , "_redirects"
  , "CORS"
  , "version.js"
  ]



// 📣


self.addEventListener("install", event => {
  const promise = removeAllCaches()
    .then(_ => fetch("/tree.json"))
    .then(response => response.json())
    .then(tree => {
      const filteredTree = tree.filter(t => !exclude.find(u => u === t))
      const whatToCache = [""].concat(filteredTree).map(p => "/" + p)
      return caches.open(KEY).then(c => c.addAll(whatToCache))
    })

  event.waitUntil(promise)
})


self.addEventListener("fetch", event => {
  const isNotLocal =
    !event.request.url.match(new RegExp("^https?\:\/\/127.0.0.1")) &&
    !event.request.url.match(new RegExp("^https?\:\/\/localhost"))

  const isInternal =
    !!event.request.url.match(new RegExp("^" + self.location.origin))

  const isOffline =
    !self.navigator.onLine

  if (isNotLocal && isInternal && isOffline) {
    const promise = caches
      .match(event.request)
      .then(r => r || fetch(event.request))

    event.respondWith(promise)

  } else if (event.request.url.includes("service_worker_authentication")) {
    const [_, token] = event.request.url.split("service_worker_authentication=")
    const newHeaders = new Headers()

    for (const h of event.request.headers.entries()) {
      switch (h[0]) {
        case "range":
          newHeaders.append(h[0], h[1])
      }
    }

    newHeaders.set("Authorization", "Basic " + token)

    const newRequest = new Request(event.request, { headers: newHeaders })
    event.respondWith(fetch(newRequest))

  }
})



// ⚗️


function removeAllCaches() {
  return caches.keys().then(keys => {
    const promises = keys.map(k => caches.delete(k))
    return Promise.all(promises)
  })
}
