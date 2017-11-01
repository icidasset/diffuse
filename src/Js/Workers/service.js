importScripts("/vendor/service-cache.js");


//
// Construct

self.addEventListener("install", event => {
  const promise = fetch("/tree.json")
    .then(response => response.json())
    .then(tree => {
      const whatToCache = ["", ...tree].map(p => "/" + p);
      return caches.open("isotach").then(c => c.addAll(whatToCache));
    });

  event.waitUntil(promise);
});


self.addEventListener("fetch", event => {
  const promise = caches
    .match(event.request)
    .then(r => r || fetch(event.request));

  event.respondWith(promise);
});
