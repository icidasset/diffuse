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


const GOOGLE_DRIVE = "https://www.googleapis.com/drive/"



// 🙈


const isNativeWrapper = location.host === "localhost:44999" || location.host === "127.0.0.1:44999"
let googleDriveToken



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
  } else if (event.request.url.includes("bearer_token=")) {
    const url = new URL(event.request.url)
    const token = url.searchParams.get("bearer_token")

    if (url.href.startsWith(GOOGLE_DRIVE)) googleDriveToken = token

    url.searchParams.delete("bearer_token")
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
        ? network(event)
        : cacheThenNetwork(event)
    )

  } else if (event.request.url && event.request.url.startsWith(GOOGLE_DRIVE) && event.request.url.includes("alt=media")) {
    // For some reason Safari starts using the non bearer-token URL while playing audio
    googleDriveToken
      ? newRequestWithAuth(
        event,
        event.request.url.toString(),
        "Bearer " + googleDriveToken
      )
      : event.respondWith(
        network(event)
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


function newRequestWithAuth(event, urlWithoutToken, authToken) {
  const request = event.request
  const newHeaders = Object.fromEntries(
    request.headers.entries()
  )

  newHeaders["authorization"] = authToken

  const newRequest = new Request(
    new Request(urlWithoutToken, event.request),
    {
      headers: newHeaders,
      credentials: request.credentials,
      cache: request.cache,
      destination: request.destination,
      method: request.method,
      mode: request.mode,
      redirect: request.redirect,
      referrer: request.referrer,
    }
  )

  let retries = 0

  const makeFetch = () => fetch(newRequest).then(r => {
    if (r.ok) {
      retries = 0
      return r
    } else {
      return r.text().then(text => {
        throw new Error(text)
      })
    }
  }).catch(err => {
    // Safari keeps getting weird CORS errors from Google Drive, after some time they disappear 🤷‍♂️
    retries++
    if (retries <= 1000) return new Promise((resolve, reject) => setTimeout(makeFetch().then(resolve, reject), 1000))
    else throw new Error(err)

  })

  event.respondWith(
    makeFetch()
  )
}
