//
// Service worker
// (◡ ‿ ◡ ✿)
//
// This worker is responsible for caching the application
// so it can be used offline.

importScripts("/vendor/service-cache.js");
importScripts("/version.js");


const KEY =
  "isotach-" + self.VERSION;


const exclude =
  [ "_headers"
  , "_redirects"
  , "CORS"
  , "version.js"
  ];


//
// Construct

self.addEventListener("install", event => {
  const promise = removeAllCaches()
    .then(_ => fetch("/tree.json"))
    .then(response => response.json())
    .then(tree => {
      const filteredTree = tree.filter(t => !exclude.find(u => u === t));
      const whatToCache = ["", ...filteredTree].map(p => "/" + p);
      return caches.open(KEY).then(c => c.addAll(whatToCache));
    });

  event.waitUntil(promise);
});


self.addEventListener("fetch", event => {
  const isInternal =
    !!event.request.url.match(new RegExp("^" + self.location.origin));

  const isOffline =
    !self.navigator.onLine;

  if (isInternal && isOffline) {
    const promise = caches
      .match(event.request)
      .then(r => r || fetch(event.request));

    event.respondWith(promise);
  }
});



//
// Install

function removeAllCaches() {
  return caches.keys().then(keys => {
    const promises = keys.map(k => caches.delete(k));
    return Promise.all(promises);
  });
}
