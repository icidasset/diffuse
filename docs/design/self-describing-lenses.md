# Design: Self-describing saved data with panproto lenses

Status: **Decision-record / largely implemented**. Envelope, migration engine, and
the panproto connection are in place. Open items: browser-build WASM bundling
verification and a first real schema migration.

## Schema identity: the NSID, not an integer version

Following the atproto spec ("Lexicon Evolution"), there is **no numeric schema
version** in a lexicon — the NSID is the schema's identity. Compatible evolution
(add optional fields) keeps the same NSID and needs no migration; a breaking change
(rename/remove a field, change a type) uses a **new NSID**, with an authored lens
from the old NSID to the new one. Diffuse's envelope records `$schema` (the current
NSID) plus an ordered `$schemaHistory` of NSID transitions.

The `lexicon` integer in a lexicon file is the *language* version (always `1`), not
a schema revision.

## Envelope

```jsonc
{
  "$schema": "sh.diffuse.output.track2",      // current NSID (identity)
  "$schemaHistory": [
    {
      "from": "sh.diffuse.output.track",        // prior NSID
      "to":   "sh.diffuse.output.track2",
      "lens": {                                 // portable DSL document (steps)
        "id": "track-to-track2",
        "source": "sh.diffuse.output.track",
        "target": "sh.diffuse.output.track2",
        "steps": [
          { "rename_field": { "old": "duration", "new": "durationMs" } },
          { "add_field": { "parent": "sh.diffuse.output.track2:body", "name": "gain", "kind": "number" } }
        ]
      },
      "complement": null                        // opaque bytes for lossless write-back
    }
  ],
  "data": [ /* current-shape records */ ]
}
```

`$schemaHistory` is unbounded: every authored NSID transition appends one entry, so
an older app can walk back to its own shape and write back.

## Component coverage

The envelope is produced at the encoder, not the storage element:

| Encoder / output | Stored format | Where the envelope lives |
|---|---|---|
| `string/json` | JSON string | whole envelope |
| `bytes/json` | JSON bytes | whole envelope |
| `bytes/s3`, `bytes/dropbox` | raw bytes | transparent pass-through of envelope bytes |
| `polymorphic/indexed-db` | IDB object | `encodeCollection` for arrays; bytes pass through |
| `bytes/dasl-sync` | CBOR `Container` | `$schema` on the container |
| `bytes/automerge` | Automerge binary | `$schema` field in the CRDT doc |
| `raw/atproto-space` (records) | atproto records | `$type` = NSID on each record |
| `raw/atproto-space` (blob bundles) | CBOR blob | whole envelope |

## Modules

- `src/common/self-describing.js` — envelope `wrap`/`unwrap`/`isSelfDescribing`;
  `collectionSchema(name)` returns each collection's current NSID.
- `src/common/lens-registry.js` — `register()`/`resolve(fromNsid, toNsid)`: the
  app-bundled lenses (resolved first), falling back to embedded history lenses.
- `src/common/lens.js` — pure-JS migration engine: `project(records, lens)`,
  `migrate(...)`, `migrateEnvelope(value, name, resolve)`; plus shared
  `encodeCollection`/`decodeCollection`/`encodeJsonCollection`/`decodeJsonCollection`,
  and `writeBack(editedRecord, { lens, toLexicon, complement })` — the write-back
  path.
- `src/common/panproto.js` — the `@panproto/core` (WASM) connection point, loaded
  lazily; `compileLens`/`parseLexicon`/`get`/`put` for the lossless complement
  write-back path.

panproto's WASM is loaded **only on write-back**: `writeBack` uses the pure-JS
`project` when no complement is involved, and calls panproto `put` (lazily loading
`@panproto/core`) only when a stored complement must preserve discarded fields.
Ordinary reads never touch panproto.

## Lens-source resolution (two tiers)

1. **App-bundled registry** — lenses shipped with the build (cheap, well-known).
2. **Embedded in `$schemaHistory`** — required for old-app-reads-newer-data: an old
   app cannot know lenses authored after it shipped, so the segment it must traverse
   is embedded.

## Read / write paths

- Any app reads a payload whose `$schema` matches its own: parse `data` directly.
- A stale payload (different NSID) is projected to the current NSID on read
  (`migrateEnvelope`); the transition is appended to `$schemaHistory`.
- `src/common/lens.js` exposes `writeBack(record, { lens, toLexicon, complement })`
  and `writeBackCollection(items, name, storedEnvelope, toLexicon)` for lossless
  write-back via panproto `put` (the `complement` preserves discarded fields;
  panproto is lazy-imported only when a complement is actually used). For diffuse's
  additive/rename migrations the pure-JS `project` suffices and panproto is never
  loaded.

### Write-back gating

Lossless write-back to a **newer** shape requires the newer lexicon (`toLexicon`)
to instantiate the lens — which an *older* build that only knows the old shape does
not have. So write-back is practical at a migration site that holds *both* the newer
lexicon and the older data being lifted forward, not from an old writer against a
newer stored shape. `writeBackCollection` is a no-op (records pass through, no WASM)
until a caller supplies a complement-carrying history entry **and** the target
lexicon; this becomes live at the first real schema migration.

## Validation

`deno check src` / `deno check specs`, and the doc-test suite (`deno task test:doc`)
cover `self-describing.js`, `lens-registry.js`, `lens.js`, and `panproto.js`.
Browser integration tests (astral/Chromium) cannot run in every sandbox.

## Open items

- Verify Lume/esbuild bundles `@panproto/core`'s WASM (`deno task build`) — it is a
  dependency and `panproto.js` is the lazy import point; the build integration needs
  checking in a normal dev environment.
- Update `deno.lock`/vendor for the new dependency (blocked by sandbox FS limits
  here — vendoring writes to Deno's global npm cache).
- Author the first real NSID migration (with actual old + new lexicons) to exercise
  `migrateEnvelope` + panproto write-back end to end.

See `docs/guides/updating-lexicons.md` for the hands-on workflow.