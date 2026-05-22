# Diffuse

_Diffuse is a few things:_

- A collection of custom DOM elements (aka. framework-agnostic web components) that allow you to build an audio player, do audio metadata processing and browsing, list audio files and streams from various sources/APIs/servers, define how to save or sync user-data, etc.
- A HTML loader that loads "interfaces" from the local user-data cache, the default set from Diffuse, HTTP(S) URLs, or [AT Protocol](https://atproto.com) URIs. When you load an interface, it's supplied with optional "features" which are also pieces of HTML. These pieces are called "facets" and are loaded from the local user-data cache as well.
- A default configuration of the custom elements that's called "the foundation". This is used throughout the default set of facets. This foundation is configured so that the facets can communicate across tabs/frames; this is done by setting the element's `group` attribute.
- A collection of data schemas in the form of atproto [lexicons](https://atproto.com/specs/lexicon). These live in the `lexicons` directory.

Putting all of this together to make cooperative and [malleable](https://www.inkandswitch.com/essay/malleable-software/) software. More information on the [website](https://elements.diffuse.sh/).


## Developer usage

You can either consume the Diffuse library via the [deployed instance](https://elements.diffuse.sh/elements/) (the listed elements link to Javascript files) or the [Javascript package](https://jsr.io/@toko/diffuse). From there you can use the custom elements as with any other custom DOM element, by writing HTML or creating a `Class` instance.

```html
<script src="https://elements.diffuse.sh/latest/components/engine/queue/element.js"></script>

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
