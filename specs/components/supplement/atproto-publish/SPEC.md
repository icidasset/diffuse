# Publish on the Atmosphere

This element publishes a copy of the user's playlist items, facets and
collaborations to their AT Protocol account. It is **not** an output: it reads
from whichever output is selected and publishes the data beside it. A prelude
facet mounts it, so it can run alongside S3, Dropbox, the local output, and so
on.

## Requirements

- It must not require being selected as the user's output, and it must not
  change the selected output.
- It owns the shared AT Protocol publish session. The connect facet is the UI
  for signing in and out; the prelude facet runs headlessly beside any output.
  Running the element without a session is a no-op.
- The session uses the stable OAuth client metadata at
  `components/output/raw/atproto-passkey/oauth-client-metadata.json`. That path
  must keep serving the same metadata even though there is no longer an output
  element there, because its URL is the `client_id` of existing sessions.
- Playlist items are stored as `sh.diffuse.output.playlistItemBundle` records,
  one per playlist, with the items CBOR-encoded into a blob. The record key is
  derived from the playlist name, so it matches what other Diffuse clients
  write.
- Facets are stored as `sh.diffuse.output.facet` records keyed by their `id`.
- Collaborations are stored as `sh.diffuse.output.collaboration` records keyed
  by their `id`, so other accounts can discover shared playlists even when the
  user's own output is not AT Protocol.
- Ephemeral playlist items are never published.
- Only changed records are written. The element remembers what it published in
  `localStorage`, per account, so a reload does not re-upload blobs.
- Deleting locally deletes the corresponding record remotely, but **only** for
  records this element published itself. Records written by another client are
  never deleted.
- Saves are debounced, and writes are serialised, so a burst of local changes
  cannot flood the PDS.
