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
