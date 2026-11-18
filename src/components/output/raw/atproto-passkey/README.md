# OAuth client metadata (stable URL)

This is the AT Protocol OAuth client metadata for the **Publish on the
Atmosphere** feature.

It intentionally stays at this path even though there is no longer an output
element in this directory: the `client_id` in the JSON is this file's public
URL, and that URL is part of existing users' OAuth sessions. Moving or renaming
the file would invalidate those sessions.

The shared client implementation lives in `src/oauth/atproto.js`.
