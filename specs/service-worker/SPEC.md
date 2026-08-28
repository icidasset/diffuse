# Service worker

## Requirements

- It must be able to serve every resource offline that has been requested before.
- Every file must be cached by CID.
- The file tree (a map of path → CID for file) is embedded directly in the service worker at build time. Because every build produces a new service worker, the embedded tree is always current.
- Every new build means a new service worker.
- All pages must use the same code.
- When a new service worker is available, install and activate it immediately, while still making sure all pages are on the same service worker. This is done through sending a `{ type: "sw-activated" }` message to the client, on which they must reload the page.
- Media and partial requests should never be cached.
- When fetching a resource from the network, use a `cache: "no-cache"` to bypass the cache (to ensure we always got the latest content).
- When the connection is offline and we don't have the content cached, return a `503` error.
- For items that are listed in the file tree, prefer the cache first and the network second. Otherwise network first if online.
- When a network request is made, cache the result.
- Cross-origin requests are cached too.
- Non-GET requests are not cached.
- When making a network-first request, when that returns an error or the resource is not found; do not fallback to cache.

### Credentials

The service worker must be able to translate requests with the `diffuse:basic-auth` query parameter and requests with user credentials in the URL (eg. `user:pass@host`). The credentials are moved to a `Authorization: Basic` header. These requests aren't cached.
