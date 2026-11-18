# Publish on the Atmosphere

This element publishes a copy of the user's playlist items, facets and
collaborations to their AT Protocol account. It is **not** an output: it reads
from whichever output is selected and publishes the data beside it. A prelude
facet mounts it, so it can run alongside S3, Dropbox, the local output, and so
on.

## Requirements

- It must not require being selected as the user's output, and it must not
  change the selected output.
- It reuses the public AT Protocol output's OAuth session (same client, scopes
  and stored session) so users do not sign in twice. Running it without a
  session is a no-op.
- Playlist items are stored as `sh.diffuse.output.playlistItemBundle` records,
  one per playlist, with the items CBOR-encoded into a blob. The record key is
  derived from the playlist name the same way the public output derives it, so
  both elements address the same record.
- Facets are stored as `sh.diffuse.output.facet` records keyed by their `id`.
- Collaborations are stored as `sh.diffuse.output.collaboration` records keyed
  by their `id`, so other accounts can discover shared playlists even when the
  user's own output is not AT Protocol.
- Ephemeral playlist items are never published.
- Only changed records are written. The element remembers what it published in
  `localStorage`, per account, so a reload does not re-upload blobs.
- Deleting locally deletes the corresponding record remotely, but **only** for
  records this element published itself. Records written by another client or by
  the public output are never deleted.
- When the selected output already stores the data in the public AT Protocol
  repo, publishing is skipped to avoid redundant writes. The AT Protocol space
  output is not skipped: its data lives in a space, not the public repo.
- Saves are debounced, and writes are serialised, so a burst of local changes
  cannot flood the PDS.
