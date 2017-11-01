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

  const response =
    isInternal

    // Cache internal requests
    ? caches
        .match(event.request)
        .then(r => r || fetch(event.request))

    // Enable CORS for outgoing requests
    : fetch(new Request(event.request.url, { mode: "cors" }))

  // Respond
  event.respondWith(response);
});
