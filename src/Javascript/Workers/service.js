//
// Service worker
// (◡ ‿ ◡ ✿)
//
// This worker is responsible for caching the application
// so it can be used offline.


const KEY =
  /* eslint-disable no-undef */
  `diffuse-${BUILD_TIMESTAMP}`


const EXCLUDE =
  [ "_headers"
  , "_redirects"
  , "CORS"
  ]


const isNativeWrapper = location.host === "localhost:44999" || location.host === "127.0.0.1:44999"



// 📣


self.addEventListener("activate", _event => {
  // Remove all caches except the one with the currently used `KEY`
  caches.keys().then(keys => {
    keys.forEach(k => {
      if (k !== KEY) caches.delete(k)
    })
  })
})


self.addEventListener("install", event => {
  if (isNativeWrapper) {
    return self.skipWaiting()
  }

  const href = self.location.href.replace("service-worker.js", "")
  const promise = fetch("tree.json")
    .then(response => response.json())
    .then(tree => {
      const filteredTree = tree.filter(t => !EXCLUDE.find(u => u === t))
      const whatToCache = [ href, `${href.replace(/\/+$/, "")}/about/` ].concat(filteredTree)
      return caches.open(KEY).then(c => Promise.all(whatToCache.map(x => c.add(x))))
    })

  event.waitUntil(promise)
})


self.addEventListener("fetch", event => {
  const isInternal =
    !!event.request.url.match(new RegExp("^" + self.location.origin))

  // When doing a request with basic authentication in the url, put it in the headers instead
  if (event.request.url.includes("service_worker_authentication=")) {
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

  // Use cache if internal request and not using native app
  } else if (isInternal) {
    event.respondWith(
      isNativeWrapper
        ? cacheThenNetwork(event)
        : network(event)
    )

  }
})


function cacheThenNetwork(event) {
  let url = new URL(event.request.url)
  url.search = ""

  return caches
    .open(KEY)
    .then(cache => cache.match(url))
    .then(match => match || fetch(url))
}


function network(event) {
  return fetch(event.request.url)
}


addEventListener("message", event => {
  if (event.data === "skipWaiting") {
    skipWaiting()
  }
})



// ⚗️


function newRequestWithAuth(event, urlWithoutToken, authToken, mode) {
  const newHeaders = new Headers(event.request.headers)
  newHeaders.set("authorization", authToken)

  const newRequest = new Request(new Request(urlWithoutToken, event.request), {
    headers: newHeaders,
    mode: mode || "cors",
    cache: "no-cache"
  })

  event.respondWith(fetch(newRequest))
}
