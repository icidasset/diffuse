//
// Service
// (◡ ‿ ◡ ✿)
//
// This code is responsible for caching the application
// so it can be used offline.

importScripts("/vendor/service-cache.js");
importScripts("/version.js");


const KEY =
  "isotach-" + self.VERSION;


const exclude =
  [ "_headers"
  , "_redirects"
  , "CORS"
  ];


//
// Construct

self.addEventListener("install", event => {
  const promise = fetch("/tree.json")
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
    !!event.request.url.match(new RegExp("^" + self.registration.scope));

  if (isInternal) {
    event.respondWith(
      caches
        .match(event.request)
        .then(r => r || fetch(event.request))
    );
  }
});
