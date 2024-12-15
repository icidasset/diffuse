//
// Service worker
// (◡ ‿ ◡ ✿)
//
// This worker is responsible for caching the application
// so it can be used offline and acts as a proxy that
// allows for example, authentication through headers
// when using audio elements.
//
/// <reference lib="webworker" />


const KEY =
  /* eslint-disable no-undef *//* @ts-ignore */
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


self.addEventListener("activate", () => {
  // Remove all caches except the one with the currently used `KEY`
  caches.keys().then(keys => {
    keys.forEach(k => {
      if (k !== KEY) caches.delete(k)
    })
  })
})


self.addEventListener("install", event => {
  if (isNativeWrapper) {
    return globalThis.skipWaiting()
  }

  const href = self.location.href.replace("service-worker.js", "")
  const promise = fetch("tree.json")
    .then(response => response.json())
    .then(tree => {
      const filteredTree = tree
        .filter(t => !EXCLUDE.find(u => u === t))
        .filter(u => u.endsWith(".jpg"))
      const whatToCache = [ href, `${href.replace(/\/+$/, "")}/about/` ].concat(filteredTree)
      return caches.open(KEY).then(c => Promise.all(whatToCache.map(x => c.add(x))))
    })

  event.waitUntil(promise)
})


self.addEventListener("fetch", event => {
  const isInternal =
    !!event.request.url.match(new RegExp("^" + self.location.origin))

  // Ping
  if (event.request.url.includes("?ping=1")) {
    event.respondWith(
      (async () => {
        const serverIsOnline = await network(event).then(_ => true).catch(_ => false)
        return new Response(JSON.stringify(serverIsOnline), {
          headers: { "Content-Type": "application/json" }
        })
      })()
    )

  // When doing a request with basic authentication in the url, put it in the headers instead
  } else if (event.request.url.includes("basic_auth=")) {
    const url = new URL(event.request.url)
    const token = url.searchParams.get("basic_auth")

    url.searchParams.delete("basic_auth")
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
  const url = new URL(event.request.url)
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
    globalThis.skipWaiting()
  }
})



// ⚗️


function newRequestWithAuth(event: FetchEvent, urlWithoutToken: string, authToken: string) {
  const request = event.request
  const newHeaders = new Headers(event.request.headers)
  newHeaders.append("authorization", authToken)

  const newRequest = new Request(
    new Request(urlWithoutToken, event.request),
    {
      headers: newHeaders,
      credentials: request.credentials,
      cache: request.cache,
      method: request.method,
      mode: request.mode,
      redirect: request.redirect,
      referrer: request.referrer,
    }
  )

  const makeFetch = () => fetch(newRequest).then(r => {
    if (r.ok) {
      return r
    } else {
      return r.text().then(text => {
        throw new Error(text)
      })
    }
  })

  event.respondWith(
    makeFetch()
  )
}
