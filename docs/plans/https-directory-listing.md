# HTTPS directory listing (`https+json`)

## Context

Some HTTPS servers expose a directory listing API: appending `?ls` to a directory URL returns a JSON array describing its contents:

```json
[
  { "name": "my-directory", "type": "directory", "mtime": "2022-10-07T00:53:50Z" },
  { "name": "my_file.tar.gz", "type": "file", "mtime": "2022-09-27T22:44:34Z", "size": 332 }
]
```

Rather than bolting this onto the existing HTTPS input (which is designed for individual file URLs), this feature is implemented as a separate `https+json` input with its own URI scheme. Users add a server URL (e.g. `https+json://music.example.com`) and the input automatically discovers all audio files under it — recursively traversing subdirectories.

The existing HTTPS input is left untouched.

## Plan

### 1. Create `src/components/input/https-json/constants.js`

```js
export const SCHEME = "https+json";
```

### 2. Create `src/components/input/https-json/common.js`

Follows the same structure as `https/common.js`. Key additions are `buildURI` and `listDirectory`.

**`parseURI(uriString)`** — validates the `https+json:` scheme and returns components:

```js
export function parseURI(uriString) {
  try {
    const url = new URL(uriString);
    if (url.protocol !== "https+json:") return undefined;

    return {
      // The real HTTPS URL (for fetch calls)
      url: "https:" + uriString.slice("https+json:".length),
      domain: url.hostname,
      host: url.host,
      path: url.pathname,
    };
  } catch {
    return undefined;
  }
}
```

**`buildURI(host, path)`** — constructs an `https+json://` URI:

```js
export function buildURI(host, path = "/") {
  return `https+json://${host}${path}`;
}
```

**`listDirectory(httpsUrl)`** — fetches `url?ls`, parses the JSON listing, and recursively collects audio file URLs. Returns `https+json://` URIs (not plain `https://`):

```js
export async function listDirectory(httpsUrl) {
  let entries;

  try {
    const response = await fetch(httpsUrl + "?ls");
    if (!response.ok) return [];
    entries = await response.json();
  } catch {
    return [];
  }

  if (!Array.isArray(entries)) return [];

  const results = await Promise.all(
    entries.map(async (entry) => {
      if (entry.type === "file") {
        if (!isAudioFile(entry.name)) return [];
        const fileHttpsUrl = httpsUrl + entry.name;
        // Return as https+json:// URI
        return [fileHttpsUrl.replace(/^https:/, "https+json:")];
      } else {
        return listDirectory(httpsUrl + entry.name + "/");
      }
    }),
  );

  return results.flat(1);
}
```

Include `groupTracksByHost`, `groupUrisByHost`, `hostsFromTracks` — same implementations as in `https/common.js` but using `parseURI` from this module. Include `consultHostCached` — same implementation, caches per host.

### 3. Create `src/components/input/https-json/worker.js`

Five standard actions. Mirrors the S3 worker pattern for `list()`.

**`consult`** — same shape as HTTPS: scheme-only returns `undetermined`, full URI checks host reachability via `consultHostCached`.

**`detach`** — same shape as HTTPS: groups by host, removes the target host's group.

**`groupConsult`** — same shape as HTTPS: groups URIs by host, checks each host once.

**`list(cachedTracks)`** — the key new logic:

```js
export async function list(cachedTracks = []) {
  // Build a URI → track cache for preserving metadata across relists.
  /** @type {Record<string, Track>} */
  const cacheByUri = {};
  cachedTracks.forEach((t) => { cacheByUri[t.uri] = t; });

  // Group by host; derive the scan root per host.
  // If placeholder tracks exist for a host, use their path as the root.
  // Otherwise, find the common path prefix of the audio file tracks.
  const groups = groupTracksByHost(cachedTracks);

  const promises = Object.values(groups).map(async ({ host, tracks }) => {
    const root = scanRoot(tracks); // "/" or a shared directory prefix
    const rootHttpsUrl = `https://${host}${root}`;
    const now = new Date().toISOString();

    const uris = await listDirectory(rootHttpsUrl);

    let discovered = uris.map((uri) => {
      const cached = cacheByUri[uri];
      /** @type {Track} */
      return {
        $type: "sh.diffuse.output.track",
        id: cached?.id ?? TID.now(),
        createdAt: cached?.createdAt ?? now,
        updatedAt: cached?.updatedAt ?? now,
        stats: cached?.stats,
        tags: cached?.tags,
        uri,
      };
    });

    if (!discovered.length) {
      discovered = [{
        $type: "sh.diffuse.output.track",
        id: TID.now(),
        createdAt: now,
        updatedAt: now,
        kind: "placeholder",
        uri: buildURI(host, root),
      }];
    }

    return discovered;
  });

  return (await Promise.all(promises)).flat(1);
}
```

`scanRoot(tracks)` is a small helper (defined in `worker.js` or `common.js`) that:
- Returns the path of the first placeholder-kind track if one exists
- Otherwise returns the longest common path prefix of all track paths, trimmed to the last `/`

**`resolve({ uri })`** — strips the scheme prefix and returns the `https://` URL with a one-year expiry (same as HTTPS input):

```js
export async function resolve({ uri }) {
  const parsed = parseURI(uri);
  if (!parsed) return undefined;

  const expiresAt = Math.round(Date.now() / 1000) + 60 * 60 * 24 * 365;
  return { url: parsed.url, expiresAt };
}
```

### 4. Create `src/components/input/https-json/element.js`

Minimal — same shape as `https/element.js`. `sources()` returns unique hosts from tracks.

```js
class HttpsJsonInput extends DiffuseElement {
  static NAME = "diffuse/input/https-json";
  static WORKER_URL = "components/input/https-json/worker.js";

  SCHEME = SCHEME;

  constructor() {
    super();
    this.proxy = this.workerProxy();
    this.consult = this.proxy.consult;
    this.detach  = this.proxy.detach;
    this.groupConsult = this.proxy.groupConsult;
    this.list    = this.proxy.list;
    this.resolve = this.proxy.resolve;
  }

  sources(tracks) {
    return Object.values(hostsFromTracks(tracks)).map((host) => ({
      label: host,
      uri: buildURI(host),
    }));
  }
}

export const CLASS = HttpsJsonInput;
export const NAME = "di-https-json";

customElements.define(NAME, CLASS);
```

### 5. Register in `src/components/orchestrator/input/element.js`

Add the import and the element to the rendered template:

```js
import "@components/input/https-json/element.js";
// ...
render({ html }) {
  return html`
    <dc-input>
      <di-https></di-https>
      <di-https-json></di-https-json>
      <di-opensubsonic></di-opensubsonic>
      <di-s3></di-s3>
    </dc-input>
  `;
}
```

## Files changed (5)

| File | Change | Difficulty |
|------|--------|------------|
| `src/components/input/https-json/constants.js` | New — define `SCHEME = "https+json"` | Trivial |
| `src/components/input/https-json/common.js` | New — `parseURI`, `buildURI`, `listDirectory`, grouping helpers, `consultHostCached` | Low |
| `src/components/input/https-json/worker.js` | New — all 5 actions; `list()` does recursive `?ls` fetching | Low-medium |
| `src/components/input/https-json/element.js` | New — `HttpsJsonInput` element, `di-https-json` | Trivial |
| `src/components/orchestrator/input/element.js` | Add import + `<di-https-json>` to template | Trivial |

**Overall difficulty: Low-medium.** All patterns are direct copies or adaptations of the existing HTTPS and S3 inputs. The only genuinely new logic is `listDirectory()` (recursive `?ls` fetch) and `scanRoot()` (derive listing root from cached tracks).

## Verification

- Add `https+json://music.example.com` as a source and trigger a library refresh
- Confirm audio files from the server's directory tree appear as tracks
- Confirm cached metadata (tags, stats) is preserved across relists
- Add a server with no audio files — confirm a placeholder track is produced
- Confirm `resolve()` returns a working `https://` URL for each track
- Confirm `di-https` (plain HTTPS) is unaffected
