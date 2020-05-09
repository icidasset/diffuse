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
  ]



// 📣


self.addEventListener("activate", event => {
  event.waitUntil(self.clients.claim())
})


self.addEventListener("install", event => {
  const href = self.location.href.replace("service-worker.js", "")
  const promise = removeAllCaches()
    .then(_ => fetch("tree.json"))
    .then(response => response.json())
    .then(tree => {
      const filteredTree = tree.filter(t => !exclude.find(u => u === t))
      const whatToCache = [ href, "brain.elm.js", "ui.elm.js" ].concat(filteredTree)
      return caches.open(KEY).then(c => Promise.all(whatToCache.map(x => c.add(x))))
    })
    .then(_ => self.skipWaiting())

  event.waitUntil(promise)
})


self.addEventListener("fetch", event => {
  // const isNotLocal =
  //   !event.request.url.match(new RegExp("^https?\:\/\/127.0.0.1")) &&
  //   !event.request.url.match(new RegExp("^https?\:\/\/localhost"))

  const isInternal =
    !!event.request.url.match(new RegExp("^" + self.location.origin))

  const isOffline =
    !self.navigator.onLine

  // Use cache if offline and identified as cached (internal)
  if (isInternal && isOffline) {
    const promise = caches
      .match(event.request)
      .then(r => r || fetch(event.request))

    event.respondWith(promise)

  // When doing a request with basic authentication in the url, put it in the headers instead
  } else if (event.request.url.includes("service_worker_authentication=")) {
    const url = new URL(event.request.url)
    const token = url.searchParams.get("service_worker_authentication")

    url.searchParams.delete("service_worker_authentication")
    url.search = "?" + url.searchParams.toString()

    newRequestWithAuth(
      event,
      url.toString(),
      "Basic " + token
    )

  // When doing a request with access token in the url, put it in the headers instead
  } else if (event.request.url.includes("access_token=")) {
    const url = new URL(event.request.url)
    const token = url.searchParams.get("access_token")

    url.searchParams.delete("access_token")
    url.search = "?" + url.searchParams.toString()

    newRequestWithAuth(
      event,
      url.toString(),
      "Bearer " + token
    )

  }
})



// ⚗️


function newRequestWithAuth(event, urlWithoutToken, authToken, mode) {
  const newHeaders = new Headers(event.request.headers)
  newHeaders.set("authorization", authToken)

  const newRequest = new Request(new Request(urlWithoutToken, event.request), {
    headers: newHeaders,
    mode: mode || "cors",
    cache: "no-cache",
    credentials: "include"
  })

  event.respondWith(fetch(newRequest))
}


function removeAllCaches() {
  return caches.keys().then(keys => {
    const promises = keys.map(k => caches.delete(k))
    return Promise.all(promises)
  })
}
