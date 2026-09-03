# Updating a lexicon (schema change) & migrating stored data

This is the workflow for changing a diffuse schema lexicon and getting stored data
auto-migrated to the new shape.

## When you need this

Diffuse persists four collections per output — `facets`, `playlistItems`, `settings`,
`tracks` — each governed by an atproto lexicon in `lexicons/output/`
(e.g. `sh.diffuse.output.facet`, `sh.diffuse.output.track`). If you change a
lexicon's shape, existing stored data is in the **old** shape. This guide wires
that migration.

### How atproto versions lexicons (the model we follow)

In keeping with the atproto spec ("Lexicon Evolution"), the atproto community does
**not** put an integer version in a lexicon. The rules:

- **The lexicon NSID is the schema's identity.** There is no numeric schema version.
- **Compatible evolution keeps the same NSID**: new fields must be optional; never
  remove non-optional fields (keep them and mark deprecated); never change types or
  rename fields. Old data stays valid under the updated lexicon, and new data stays
  valid under the old one.
- **Breaking changes use a NEW NSID** (e.g. `sh.diffuse.output.track2`), not a version
  bump on the same NSID.
- The `lexicon` field in a lexicon file is the **language** version (always `1`), not
  a schema revision. Leave it alone.

So "moving to the new shape" is expressed as a lens **from one NSID to another**
(possibly the same NSID, when the change is purely compatible). Diffuse's envelopes
record the NSID (`$schema`) plus an ordered `$schemaHistory` of NSID transitions.

The self-describing machinery that does the work lives in:

- `src/common/self-describing.js` — the `{ $schema, $schemaHistory, data }` envelope,
  `wrap`/`unwrap`, and `COLLECTION_SCHEMAS` (each collection's current lexicon NSID).
- `src/common/lens-registry.js` — `register()` / `resolve()` for authored lens
  documents (the "lenses in app code" tier).
- `src/common/lens.js` — `project()`, `migrate()`, `migrateEnvelope()`; the engine
  that applies a lens's steps to project records between NSIDs.
- Each output encoder's read path (indexed-db, `bytes/json`, `string/json`,
  `bytes/dasl-sync`, `bytes/automerge`, atproto-space blob bundles) calls
  `migrateEnvelope` so stale payloads are migrated on read.

## Steps

### 1. Make the schema change

Two cases:

- **Compatible change** (add an optional field): edit the existing lexicon in
  `lexicons/output/*.json`. Keep the same NSID; no lens needed (old data remains
  valid; the new field simply appears).
- **Breaking change** (rename/remove a field, change a type): create a **new lexicon
  file** with a new NSID (e.g. `sh.diffuse.output.track` → `sh.diffuse.output.track2`),
  and keep the old NSID's record fields available for migration. You'll author a lens
  from the old NSID to the new one.

### 2. Regenerate the TypeScript types

The generated types in `src/definitions/types/` come from the lexicons:

```sh
deno task gen:defs:types
```

(this runs `@atcute/lex-cli generate` plus the replace/strip helpers). The
`Main`/`Facet`/`Track`… types consumed across the codebase must match the new shape.

### 3. Point the collection at the current NSID

In `src/common/self-describing.js`, set the collection's entry in `COLLECTION_SCHEMAS`
to the **current** NSID. For a breaking change this is the new NSID; for a purely
compatible change it stays the same.

```js
const COLLECTION_SCHEMAS = {
  // ...
  tracks: "sh.diffuse.output.track2", // was "sh.diffuse.output.track"
};
```

This is the value stamped on every new save and the NSID the migration projects toward.

> If the change is purely compatible (same NSID, optional field added) you're done
> after steps 1–2: there's nothing to migrate. Skip to validation.

### 4. Author & register the lens (old NSID → new NSID)

Only needed for a **breaking change** (different NSIDs). In
`src/common/lens-registry.js`, add a lens document describing the transition. The
DSL steps supported by the projection engine are `rename_field`, `add_field`,
`remove_field`:

```js
register({
  id: "track-to-track2",
  source: "sh.diffuse.output.track",
  target: "sh.diffuse.output.track2",
  steps: [
    { rename_field: { old: "duration", new: "durationMs" } },
    { add_field: { parent: "sh.diffuse.output.track2:body", name: "gain", kind: "number" } },
    // { remove_field: { name: "obsolete" } },
  ],
});
```

Notes:

- The `source`/`target` are NSIDs. `rename_field`/`add_field`/`remove_field` step
  between their shapes; `target` (and its `:body` vertex) is the new lexicon.
- These are the reversible, pure-JS steps the migration engine applies; they cover
  the common rename/add cases. A field **discarded** by a migration (e.g.
  `remove_field`) that an *older app* must still be able to write back needs the
  panproto complement path, which is a separate, not-yet-implemented engine (see
  open items in `docs/design/self-describing-lenses.md`).

### 5. That's it — the read path migrates automatically

With the collection's current NSID updated and the lens registered, no further wiring
is needed:

- Any payload whose `$schema` is an **older NSID** is projected to the current NSID on
  read (via `migrateEnvelope`), and the NSID transition is appended to the payload's
  `$schemaHistory`.
- New saves are written under the current NSID.
- The lens is embedded in `$schemaHistory` so even an app that doesn't have it bundled
  (an older build) can traverse the transition — this is the "lenses in the data"
  correctness guarantee.

## First migration: handling existing (legacy) data

Before the envelope existed, stored data was a bare array / bare CBOR / raw IDB value
with **no recorded NSID**. The read path treats such values as belonging to the
collection's *current* NSID (no migration). For a first migration, this means legacy
bare data is treated as the **old** shape only if nothing distinguishes it — typically
you accept it as the current shape, or you avoid a breaking change until the envelope
is universally writing.

## Recommended validation before shipping

Add a unit (doc) test that exercises the exact path an old user hits, then run the
suite:

```ts
import { wrap } from "~/common/self-describing.js";
import { register, resolve } from "~/common/lens-registry.js";
import { migrateEnvelope } from "~/common/lens.js";

register({
  id: "track-to-track2", source: "sh.diffuse.output.track", target: "sh.diffuse.output.track2",
  steps: [{ rename_field: { old: "duration", new: "durationMs" } }],
});
const envelope = wrap([{ id: "t1", uri: "u", duration: 300 }],
  { schema: "sh.diffuse.output.track" });
const out = migrateEnvelope(envelope, "tracks", resolve);
// assert out.data[0] has `durationMs` and not `duration`, and out.envelope.$schema
// is "sh.diffuse.output.track2" with a $schemaHistory entry for track -> track2.
```

Then:

```sh
deno task test:doc   # unit / documentation tests
deno check src        # type checks
deno check specs
```

> Browser integration tests (`deno task test:integration`) launch a headless
> Chromium via astral; they can't run in every sandbox. If they fail with "Your
> binary refused to boot", that's environmental, not your change.

## Checklist

1. Make the change: compatible (edit same-NSID lexicon) or breaking (new lexicon NSID)
2. `deno task gen:defs:types`
3. Point the collection's `COLLECTION_SCHEMAS` entry at the current NSID
   (`self-describing.js`)
4. For breaking changes, `register()` the old-NSID → new-NSID lens
   (`lens-registry.js`)
5. Add/confirm a migration doc-test; run `deno test --doc`, `deno check src`,
   `deno check specs`
6. Confirm legacy (pre-envelope) data handling and that a migration actually triggers

See `docs/design/self-describing-lenses.md` for the full design (envelope shape,
two-tier lens resolution, read/write paths, and open items like the wasm complement
path).