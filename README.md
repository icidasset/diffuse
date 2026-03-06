# Diffuse

**Construct your audio player by composing web components.**

Diffuse provides a range of custom elements: audio input, data output, metadata & artwork processing, audio playback, a queue system, and much more.

It is also aimed at consumers, providing themes and facets, preconfigured component compositions; while simultaneously trying to be [malleable software](https://www.inkandswitch.com/essay/malleable-software/).

More information on the [website](https://elements.diffuse.sh/latest/).


## Developer usage

You can either consume the Diffuse library via the [deployed instance](https://elements.diffuse.sh/latest/) (the listed elements link to Javascript files) or the [Javascript package](https://jsr.io/@toko/diffuse). From there you can use the custom elements as with any other custom DOM element, by writing HTML or creating a `Class` instance.

```html
<script src="https://elements.diffuse.sh/bafybeiexuhqumeljxdmmsdfet5oh2h7pam6fy7gbktqfbsai5qfu2ze6hq/components/engine/queue/element.js"></script>

<de-queue></de-queue>
```

```js
import QueueEngine from "@toko/diffuse/components/engine/queue/element.js"

const queue = new QueueEngine()
queue.setAttribute("group", "facets")

document.body.append(queue)
````


## Build it yourself

Install [Deno](https://docs.deno.com/runtime/getting_started/installation/).

```shell
deno run gen:defs:types
deno run build # or deno run serve
```

Diffuse is built with:
- [Deno](https://deno.com)
- Web components (custom elements)
- Web workers (also: shared + service workers)
- Signals (currently [alien-signals](https://github.com/stackblitz/alien-signals), but hopefully [TC39](https://github.com/tc39/proposal-signals) in the future)
- [`lit-html`](https://lit.dev/docs/libraries/standalone-templates/)
- [`music-metadata`](https://github.com/Borewit/music-metadata)
- [Lume](https://lume.land) & [ESBuild](https://esbuild.github.io)
