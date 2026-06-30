# Artwork components

These components are responsible to fetch artwork for a `Track`, if possible.


## Requirements

- Every artwork component must be of the `ArtworkElement` type (see `types.d.ts`)
- The `get` action must never throw, return `null` if anything fails. This `null` means "no artwork found".
- Prefer the quick failure resolution path. For example, when the artwork fetching requires an internet connection; don't retry the fetch, return `null` immediately.
- Ideally the best quality is picked for the images, unless that image size is insanely large (ie. larger than a 4K resolution)
- Follows naming conventions specified in the top-level spec.
