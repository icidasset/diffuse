---
name: diffuse-facet
description: Create an interface or feature facet for Diffuse
user-invocable: true
version: 0.1.0
---

Create a Diffuse facet and produce the HTML ready to paste into the `create/` page.

## Step 1 — Read the docs

Use the read tool to read these files:

- `docs/architecture.txt` — system overview, facet rules, foundation API
- `docs/elements.txt` — all available custom elements with code examples
- `example/index.html` — a representative interface facet to use as a reference
- Any specific definition you need (e.g. `docs/definitions/output/track.json` for the track schema)
- `docs/definitions/index.ts` — TypeScript types for all data structures

## Step 2 — Clarify intent

If the user hasn't described what the facet should do, ask one plain-language question before proceeding.

## Step 3 — Write the facet

Facets are HTML fragments (no `<!doctype>`, `<html>`, or `<head>`). The loader injects them into `<div id="container">` and sets a `<base>` pointing at the Diffuse build root, so all relative URLs resolve from there. The import map exposes `~/` as the root alias.

### Mandatory rules

- **`foundation.ready()`** must be called on every interface facet — it removes the loading spinner. Omitting it leaves the screen stuck on loading.
- **`foundation.setup({ title })`** should be called to set the document title.
- Always check the definitions fetched in Step 1 for the exact shape of any data you access — never assume top-level fields exist. For example, track metadata lives under `track.tags.*`, not at the top level.
- Signal reader functions (`queue.now`, `queue.past`, `queue.future`, …) must be **called inside `effect()`** to be reactive.
- Do **not** import modules with top-level `await` from Worker scripts — it causes RPC messages to be dropped.
- Use **`@param` annotations above functions**, not inline `@type` in parameter lists.

### Skeleton

```html
<style>
  @import "./styles/base.css";
  @import "./styles/diffuse/facet.css";
  @import "./vendor/@phosphor-icons/web/fill/style.css"; /* or /bold/ */

  @layer base, diffuse;

  /* facet-specific styles */
</style>

<main>
  <!-- markup -->
</main>

<script type="module">
  import foundation from "~/common/foundation.js";
  import { effect } from "~/common/signal.js";

  foundation.setup({ title: "My Facet | Diffuse" });

  // wire up elements …

  foundation.ready();
</script>
```

### Standard two-column layout

```html
<main>
  <div class="facet__left">
    <a href="./dashboard/" class="diffuse-logo-container">
      <svg viewBox="0 0 902 134" width="160">
        <title>Diffuse</title>
        <use href="images/diffuse-current.svg#diffuse"></use>
      </svg>
    </a>
    <h1>Title</h1>
    <p>Description.</p>
  </div>
  <div class="facet__right">
    <!-- main content -->
  </div>
</main>
```

For a centered or full-screen layout (player, dialog, etc.) override `body` and `main` in the facet's `<style>` block directly.

### Foundation API quick reference

```js
// Engines
const audio         = await foundation.engine.audio();
const queue         = await foundation.engine.queue();
const repeatShuffle = await foundation.engine.repeatShuffle();

// Configurators
const inputCfg    = await foundation.configurator.input();
const metadataCfg = await foundation.configurator.metadata();

// Orchestrators
const output        = await foundation.orchestrator.output();
const sources       = await foundation.orchestrator.sources();
const controller    = await foundation.orchestrator.controller();
const queueAudio    = await foundation.orchestrator.queueAudio();
const mediaSession  = await foundation.orchestrator.mediaSession();
const processTracks = await foundation.orchestrator.processTracks({ disableWhenReady: false });
const favourites    = await foundation.orchestrator.favourites();
const artwork       = await foundation.orchestrator.artwork();
const scopedTracks  = await foundation.orchestrator.scopedTracks();
const autoQueue     = await foundation.orchestrator.autoQueue();
```

Though make sure to check the mentioned foundation js file for the latest code.

Typical playback bootstrap:

```js
await foundation.orchestrator.queueAudio();
await foundation.orchestrator.mediaSession();

const [audio, ctl, queue] = await Promise.all([
  foundation.engine.audio(),
  foundation.orchestrator.controller(),
  foundation.engine.queue(),
]);

await customElements.whenDefined(ctl.localName);
```

### Reactivity

Signals are used for reactivity, see the `~/common/signal.js` javascript file for the code. It's based on the alien-signals library.

```js
effect(() => {
  const track      = ctl.currentTrack();   // computed — call like a fn
  const isPlaying  = ctl.isPlaying();
  const audioState = ctl.audio();          // AudioStateReadOnly | undefined

  if (audioState) {
    const progress = audioState.progress();   // 0–1
    const current  = audioState.currentTime();
    const duration = audioState.duration();
  }

  const now    = queue.now();     // SignalReader — call like a fn
  const past   = queue.past();
  const future = queue.future();
});
```

### Audio control

```js
audio.play({ audioId: queue.now().id });
audio.pause({ audioId: queue.now().id });
audio.seek({ audioId: queue.now().id, percentage: 0.5 }); // 0–1
queue.shift();    // next track
queue.unshift();  // previous track
```

## Step 4 — Deliver

Output the complete facet HTML in a code block. Tell the user to open the `create/` page in Diffuse, paste it in, and load it.
