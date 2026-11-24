# Record Sync v1

_Incremental, history-free replication of record collections with selective
field encryption._

**Status:** draft, with a reference implementation. The output lives in
`src/components/output/raw/firkin/encrypted/record-sync/` and is exposed by the
`facets/data/record-sync` feature facet. The field cipher lives in
`src/common/record-sync/field-cipher.js`; its tests (including the vector below)
are in `tests/common/record-sync/field-cipher.test.ts`.

This is a **data format and merge profile**, not a session protocol: it defines
what a patch, snapshot, manifest and encrypted field look like. Any transport
that can move blocks and announce changes can carry it; the reference
implementation uses the Firkin log.

A collection is synced as **record patches**, with last-writer-wins per record,
tombstones for deletes, and selective field encryption. It is incremental, keeps
no operation history, and lets a server replicate and verify data without the
content key.

Encryption is **field-level only**. Patches, snapshots and manifests are stored
as plaintext CBOR; a server sees record ids, versions, field names and every
value that is not explicitly sealed in `$enc`. Whole-entry encryption is a
separate, optional profile (see _Leakage_).

## Goals

- Incremental replication: a reader fetches only entries after its cursor.
- No operation history: a snapshot is full state, and entries before it can be
  dropped. Only tombstones (and their versions) survive compaction.
- Deterministic convergence without a central writer: the same patch set
  produces the same state on every replica.
- Selective encryption: public structure and metadata, opaque sensitive fields.
- Compose with [DASL](https://dasl.ing/) for blocks/transfer, UCAN for
  authorization, and COSE for crypto.

## Non-goals

- Field-level or text merge. Concurrent edits to the **same record** are LWW;
  use per-field versions or a CRDT if that is required.
- Server-side decryption or merging of encrypted fields.
- Hiding record ids, timestamps, field names, record sizes or non-encrypted
  fields.

## Records

A record is a CBOR map with a text `id`. Collection-specific fields follow the
application's schema (JSON-compatible values). A record may carry a `$enc` map:

```jsonc
{
  "$type": "sh.diffuse.output.track",
  "id": "t1",
  "title": "Song",
  "artist": "Artist",
  "$enc": {
    "uri": "g0eiAQMEYmsxoQVMAAECAwQFBgcICQoLWCovdqJrtt_tNOg59ubBhR1D4LnqG4RKcRFIVMc5ekcDZJhGrl6CGs4yutE"
  }
}
```

`$enc[field]` is `base64url(COSE_Encrypt0)` (see _Field encryption_). The
application overlays the decrypted value and removes `$enc[field]` before
presenting the record. **Writers that cannot decrypt a field must preserve
`$enc[field]` unchanged.**

## Patches

One patch is one coalesced batch of changes in one collection:

```jsonc
{
  "v": 1,
  "at": "2026-11-23T10:00:00.000Z",
  "device": "67efbfac56ec055dbff362d1d77f85e415a4377558571a8cb65025e98e79631d",
  "put": { "t1": {/* record */} },
  "del": ["t2"],
  "order": ["t1", "t3"]
}
```

- `at` is UTC milliseconds (RFC 3339). `device` is the writer's public key hex
  (or a DID key). The pair is the patch's **version**. Implementations with
  unreliable clocks SHOULD use a hybrid logical clock in `at`; `device` breaks
  ties.
- Each `put[id]` and each `del` id takes the patch version.
- Apply a `put` iff its version is newer than the record's stored version
  **and** newer than its tombstone. Apply a `del` by storing a tombstone
  `{ at, device }` if it is newer. A newer `put` may resurrect a tombstone.
- `order` is LWW as a whole, versioned by the patch. (Fractional per-record
  positions are out of scope for v1.)
- Applying the same patch twice is a no-op; applying patches in any order
  converges.

`del` tombstones are the only history that survives compaction.

## Snapshots

```jsonc
{
  "v": 1,
  "at": "…",
  "device": "…",
  "records": { "t1": {/* record */} },
  "tombstones": { "t2": { "at": "…", "device": "…" } },
  "order": ["t1", "t3"]
}
```

A snapshot is full state plus live tombstones. Once it is durable, all entries
before it may be dropped. Tombstones may be dropped once every known device has
applied past the snapshot (use the per-device manifest sequence as the floor).

Recommended cadence: keep the newest N entries per device (32–64), and write a
snapshot entry every 32 appends. Unlike a CRDT snapshot, this one contains no
operation history.

## Transfer (DASL)

- Blocks are CIDv1; documents are DRISL/DAG-CBOR; opaque bytes use the raw
  codec. CAR bundles blocks for transfer.
- Each device writes one manifest per collection:

  ```jsonc
  {
    "v": 1,
    "collection": "tracks",
    "device": "…",
    "seq": 42,
    "entries": ["<CID>", "…"],
    "sig": "<COSE_Sign1 over the canonical manifest bytes>"
  }
  ```

- A registry maps `collection/device` to the manifest CID. A hint channel tells
  other devices that a manifest changed and may name the new manifest, letting a
  reader fetch it directly instead of re-listing the registry.
- A reader lists the collection, reads changed manifests (compare the CID it
  applied last), fetches the entries it is missing, verifies each entry against
  its CID and signature, and applies the patches.
- The reader's cursor is the list of applied entry CIDs (or a watermark per
  device). The applied record stores the manifest listing signature so an
  unchanged log costs one listing request.

This is the current Firkin log with a different payload; DASL names the block
and transfer layer, not the merge rules.

## Signatures and authorization

- Sign each patch/snapshot with **COSE_Sign1** (RFC 9052; Ed25519) over the
  canonical bytes. A server can verify authorship and integrity without any
  content key.
- In the reference Firkin transport the signed manifest commits to every entry
  CID instead, so the manifest signature is the v1 signature profile; a
  per-patch COSE_Sign1 envelope is an alternative profile.
- Authorize writes with a **UCAN** delegation for the collection's namespace
  (the current Firkin model). Read capabilities may be broader than write.
- Servers store, deduplicate, verify, replicate and GC. They do not decrypt.

## Field encryption

- Envelope: **COSE_Encrypt0** (RFC 9052), `A256GCM` (RFC 9053, COSE algorithm
  `3`). `ChaCha20/Poly1305` (algorithm `24`) is an acceptable alternative
  profile; both are 256-bit AEADs with the same post-quantum posture.
- Protected header: `{ 1: 3, 4: <key id> }` (algorithm, key id).
- Unprotected header: `{ 5: <96-bit nonce> }`.
- External AAD, encoded as CBOR:

  ```
  ["Encrypt0", <protected header bytes>, <CBOR([
    "diffuse.records.field.v1",
    "<collection>",
    "<record id>",
    "<field>"
  ])>]
  ```

  This binds a ciphertext to its position: moving it to another collection,
  record or field fails authentication.
- Nonce: 12 random bytes per sealing.
- **Stable ciphertext:** when the plaintext is unchanged, reuse the existing
  `$enc[field]` instead of re-sealing. Re-sealing with a fresh nonce changes the
  record, its CID and the log entry, even though nothing changed.
- **Key separation:** derive a dedicated 256-bit (32-byte) field key, e.g.
  `HKDF-SHA256(secret, info = "diffuse.records.field.v1")`. Do not reuse a key
  that seals log entries (the Loro output's profile); this profile never seals
  entries, so the field key is the only content key.
- **Rotation:** a new key gets a new key id. Keep old keys for decryption and
  re-seal a field on its next write; do not re-seal unchanged fields just to
  rotate.
- **No key:** expose the record with the field locked, and refuse to edit it.
  Preserve the ciphertext on save.
- **Reference defaults:** `tracks.uri` and `settings.value` are the fields
  sealed by the reference output. A playlist item carries no URI of its own (its
  criteria reference tracks), so there is nothing to seal there.

### Post-quantum posture

Symmetric cryptography is already quantum-resistant in the relevant sense:
Grover's algorithm halves the effective key length, so a 256-bit key still gives
about 128-bit security. AES-256-GCM (the default) and ChaCha20-Poly1305 are
equivalent here; the choice between them is not what makes this quantum-safe.

The exposure is asymmetric cryptography:

- **Signatures:** Ed25519 is broken by Shor. A post-quantum profile should use a
  hybrid signature, e.g. Ed25519 + **ML-DSA-65** (FIPS 204), or **SLH-DSA**
  (FIPS 205) where stateless hash-based signatures are preferred.
- **Key agreement:** any X25519/HPKE wrapping of the field key is broken by
  Shor. Use **ML-KEM-768** (FIPS 203), hybridised with X25519 (X25519 +
  ML-KEM-768).
- **COSE registrations** for these algorithms are still drafts; a hybrid profile
  defines its own algorithm identifiers until they stabilise.
- **Hashes:** SHA-256 remains fine (~128-bit post-quantum).

The field key here is derived from a shared secret (the passkey/passphrase), so
there is no asymmetric key exchange to harvest. The practical post-quantum risk
is signature forgery (authenticity), not decryption of the log. Pure-JS
implementations of ML-KEM, ML-DSA and SLH-DSA are available in
`@noble/post-quantum`.

### Leakage

Because entries are not sealed, the server (and anyone with the log) learns:
record ids, timestamps, device ids, field names, tombstones, which fields
changed, `$enc` ciphertext sizes, and every non-encrypted field. Only the values
listed in `$enc` are confidential. If the metadata itself is sensitive, use a
whole-entry encryption profile instead (or in addition) and accept that servers
cannot merge or index it; that profile is out of scope for v1.

## Test vector

- Key: 32 bytes `00 01 02 … 1f`
- Nonce: 12 bytes `00 01 02 … 0b`
- Key id: `k1`
- Collection / record / field: `tracks` / `t1` / `uri`
- Plaintext: `https://example.com/t1.mp3`

COSE_Encrypt0, base64url:

```
g0eiAQMEYmsxoQVMAAECAwQFBgcICQoLWCovdqJrtt_tNOg59ubBhR1D4LnqG4RKcRFIVMc5ekcDZJhGrl6CGs4yutE
```

Hex:

```
8347a2010304626b31a1054c000102030405060708090a0b582a2f76a26bb6dfed34e839f6e6c1851d43e0b9ea1b844a71114854c7397a4703649846ae5e821ace32bad1
```

## Open questions

- Per-field versions for concurrent edits to different fields of one record.
- Record encoding for interop: DAG-CBOR as specified, or canonical JSON
  (RFC 8785) where a service speaks JSON.
- Whether a whole-entry encryption profile is needed alongside this one.
- Tombstone compaction policy across devices with different manifest floors.
