# AT Protocol space raw output

This element implements the output element interface using a permissioned AT
Protocol data space (`sh.diffuse.atproto.space`).

## Requirements

- The four definition lexicons (`facet`, `playlistItemBundle`, `setting`,
  `trackBundle`) are used as the schema for each output type, stored as records
  in a *personal* space (authority = the user's own DID, key `self`).
- Authentication uses OAuth with a `space:` scope; the browser OAuth client is
  `@atproto/oauth-client-browser`.
- No passkey encryption is applied: space data is access-controlled rather than
  public, so records are not assembled and rebroadcast by a relay.
- Unlike the public AT Protocol output there is no firehose/relay. Reading uses
  the space sync methods (`com.atproto.space.listRecords`, `listRepoOps`,
  `getRepo`) directly from the user's PDS.
- Tracks/playlist items are stored as CBOR blob bundles, as in the public
  output, to keep the number of records (and PDS rate-limit pressure) low.
