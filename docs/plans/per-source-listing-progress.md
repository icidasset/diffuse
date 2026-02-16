# Per-source listing progress

## Context

When tracks are processed, there are two phases: **listing** (discovering tracks from each source) and **metadata** (fetching tags/stats per track). Currently only the metadata phase reports progress. The listing phase can be slow (e.g. enumerating an S3 bucket) but appears as a black box — the UI has no visibility into it.

The goal is to show per-source progress during listing, so the user sees something like "Listing sources (2/3)..." before the metadata phase kicks in.

## Architecture challenge

The call chain crosses three worker boundaries:

```
process-tracks worker  →(RPC)→  input configurator worker  →(RPC)→  per-scheme workers
```

- `process-tracks/worker.js` calls `input.list(cachedTracks)` as one opaque RPC call
- `configurator/input/worker.js` receives it, fans out to per-scheme workers via `Promise.all`
- Each scheme worker (s3, https, opensubsonic) does the actual listing

The `announce`/`listen` mechanism only communicates between a worker and **its owning element**. So announcements from the input configurator worker go to the `dc-input` element, not to the process-tracks element or worker. This means progress must be surfaced through the element layer.

## Plan

### 1. Add listing progress signal to input configurator worker

**File:** `src/components/configurator/input/worker.js`

- Import `signal`, `effect`, `announce`
- Add module-level signal: `const $listingProgress = signal({ processed: 0, total: 0 })`
- In the `list()` function, count groups and update `$listingProgress` as each source completes:

```js
export async function list({ data, ports }) {
  const groups = await groupConsult({ data, ports });
  const entries = Object.values(groups);

  $listingProgress.value = { processed: 0, total: entries.length };
  let processed = 0;

  const promises = entries.map(async ({ available, scheme, tracks }) => {
    if (!available) { ... }
    const result = await input.list(tracks);
    processed++;
    $listingProgress.value = { processed, total: entries.length };
    return result;
  });

  const nested = await Promise.all(promises);
  return nested.flat(1);
}
```

- In `ostiary()`, announce the signal and expose it via RPC:

```js
ostiary((context) => {
  rpc(context, { ..., listingProgress: $listingProgress.get });
  effect(() => announce("listingProgress", $listingProgress.value, context));
});
```

### 2. Expose listing progress from input configurator element

**File:** `src/components/configurator/input/element.js`

- Import `signal` from `@common/signal.js` and `listen` from `@common/worker.js`
- Add a `#listingProgress` signal and a public `listingProgress` getter
- Add `connectedCallback()` to set up `listen("listingProgress", ...)` on `this.workerLink()`

### 3. Proxy listing progress through the input orchestrator

**File:** `src/components/orchestrator/input/element.js`

- Add a `listingProgress` getter that delegates to `this.input.listingProgress()`

### 4. Add a phase to process-tracks progress

**File:** `src/components/orchestrator/process-tracks/types.d.ts`

- Extend the `Progress` type with a `phase` field:

```ts
export type Progress = {
  phase: "listing" | "metadata";
  processed: number;
  total: number;
};
```

**File:** `src/components/orchestrator/process-tracks/element.js`

- The element already queries `input-selector` (the `do-input` element)
- Add an `effect()` that watches `this.input.listingProgress()` while `isProcessing` is true, and maps it into `#progress` with `phase: "listing"`
- When the worker's own metadata progress arrives (via the existing `listen("progress", ...)`), set it with `phase: "metadata"`

**File:** `src/components/orchestrator/process-tracks/worker.js`

- Update `$progress` initial value to include `phase: "metadata"` (the worker only knows about metadata)

### 5. Update the UI to show the phase

**File:** `src/themes/webamp/configurators/input/element.js`

- In `#renderProcessingProgress()`, read `phase` from the progress object
- Show "Listing sources (2/3)..." during listing phase
- Show "Gathering metadata (5/10)..." during metadata phase (current behavior)

## Files changed (6)

| File | Change | Difficulty |
|------|--------|-----------|
| `src/components/configurator/input/worker.js` | Add `$listingProgress` signal, update `list()`, announce in `ostiary` | Low-medium |
| `src/components/configurator/input/element.js` | Add signal, `connectedCallback`, `listen`, expose getter | Low |
| `src/components/orchestrator/input/element.js` | Proxy `listingProgress` getter | Trivial |
| `src/components/orchestrator/process-tracks/element.js` | Watch input's listing progress, merge into phased progress | Medium |
| `src/components/orchestrator/process-tracks/worker.js` | Add `phase` to progress signal | Trivial |
| `src/components/orchestrator/process-tracks/types.d.ts` | Add `phase` to `Progress` type | Trivial |
| `src/themes/webamp/configurators/input/element.js` | Render phase-aware progress text | Low |

**Overall difficulty: Medium.** Follows existing patterns (`announce`/`listen`, signals, proxied getters) throughout. The trickiest part is step 4 — having the process-tracks element watch the input element's listing progress and merge it with the worker's metadata progress into a single unified signal.

## Verification

- Add an S3 source with enough files to make listing take a visible amount of time
- Open the Overview tab in the input configurator
- Observe "Listing sources (X/Y)..." during listing, transitioning to "Gathering metadata (X/Y)..." during metadata extraction
- Confirm the progress bar advances during both phases
