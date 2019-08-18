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


self.addEventListener("install", event => {
  const promise = removeAllCaches()
    .then(_ => fetch("tree.json"))
    .then(response => response.json())
    .then(tree => {
      const filteredTree = tree.filter(t => !exclude.find(u => u === t))
      const whatToCache = [ "", ".", "/" ].concat(filteredTree)
      return caches.open(KEY).then(c => Promise.all(whatToCache.map(x => c.add(x))))
    })

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
    const [urlWithoutToken, token] = event.request.url.split("service_worker_authentication=")

    newRequestWithAuth(
      event,
      urlWithoutToken,
      "Basic " + token
    )

  // When doing a request with access token in the url, put it in the headers instead
  } else if (event.request.url.includes("&access_token=")) {
    const [urlWithoutToken, token] = event.request.url.split("&access_token=")

    newRequestWithAuth(
      event,
      urlWithoutToken,
      "Bearer " + token
    )

  }
})



// ⚗️


function newRequestWithAuth(event, urlWithoutToken, authToken) {
  const newHeaders = new Headers()

  for (const h of event.request.headers.entries()) {
    switch (h[0]) {
      case "range":
        newHeaders.append(h[0], h[1])
    }
  }

  newHeaders.set("authorization", authToken)

  const newRequest = new Request(event.request, {
    headers: newHeaders,
    url: urlWithoutToken
  })

  event.respondWith(fetch(newRequest))
}


function removeAllCaches() {
  return caches.keys().then(keys => {
    const promises = keys.map(k => caches.delete(k))
    return Promise.all(promises)
  })
}
