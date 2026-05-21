# AT Protocol raw output

This element implements the output element interface using the AT Protocol (PDS).

## Requirements

- The definition lexicons are used as the schema for each output type (tracks, playlist items, etc).
- The AT Protocol OAuth flow is preferred as the authentication method.
- The authenticated account must be remembered across browser sessions.
- The atproto pds has strict rate limits, we must opt for data structures that take this into consideration. There can be a large amount of tracks, 25000 for example, so a bundle would probably be preferred. Same for playlist items, maybe 5000 items or more.
- Reading and writing should be done as less as possible, though we don't want to miss out on any updates.
