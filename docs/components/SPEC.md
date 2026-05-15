# Components

These web components are custom elements (DOM elements) that serve as building blocks to make web software.

## Requirements

- Components should never rely on instances of other components, except if they live in the 'orchestrator' category or if the instance is a child of itself. Orchestrator components can refer to other components using a DOM selector, this can be provided via an attribute ending with `-selector` as its name.
- Components rely on signals exposed via the `common/signal.js` module. Only signal getters are exposed, never the entire signal object.
- A base class should be built that all components build on. This should be made available via `common/element.js`.
- Components should be broadcastable, meaning that various instances of that component should be able to communicate with each other. This will be used to replicate state and call methods. Broadcasting should have two modes: 'replicate' and 'leaderOnly'. The former is executed on all instances, while the latter is only executed on the leader. Another base class should be provided for broadcastable components. A `group` attribute will be used to choose the group in which we'll broadcast, so can we can form groups of instances that communicate.
- Components live in the `components` directory.
- Another special category of components is 'configurator', those components take DOM children of the same category. For example, A configurator must have at least the same properties as the component provided via the DOM, but may have more.
- If heavy computation work or other work which could block the main thread is needed, the element should be accompanied by a web worker.
- Register the custom element as soon as possible.
- When any namespace is needed, prefer to prefix with `diffuse/`
- When code is shared between files from a specific component, components in a specific category, or components across different categories; put it in a `common.js` file (or `common` directory with multiple js files if it gets too big) on the appropriate level.

## Other categories

### Artwork

Various ways to fetch artwork for tracks. Must adhere to API:

```ts
import type { Track } from "~/definitions/types.d.ts";

export type Actions = {
  get(track: Track): Promise<Uint8Array | null>;
};
```


### Engine

All kinds of core behaviour.


### Input

Input components generate tracks and resolve track URIs into audio. They must be of the following `InputElement` type:

```ts
import type { DiffuseElement } from "~/common/element.js";
import type { ProxiedActions } from "~/common/worker.d.ts";
import type { Track } from "~/definitions/types.d.ts";

/**
 * Consultation.
 *
 * `consult` can be "undetermined" if only a scheme was given instead of a full URI.
 */
export type Consult =
  | { supported: false; reason: string }
  | { supported: true; consult: "undetermined" | boolean };

export type ConsultGrouping =
  | { available: false; reason: string; scheme: string; uris: string[] }
  | { available: true; scheme: string; uris: string[] };

export type GroupConsult = Record<string, ConsultGrouping>;

export type InputActions = {
  artwork(uri: string): Promise<Uint8Array | null>;
  consult(uriOrScheme: string): Promise<Consult>;
  detach(args: { fileUriOrScheme: string; tracks: Track[] }): Promise<Track[]>;
  groupConsult(uris: string[]): Promise<GroupConsult>;
  list(tracks: Track[]): Promise<Track[]>;
  resolve(args: { method?: string; uri: string }): Promise<ResolvedUri>;
};

export type InputElement =
  & DiffuseElement
  & InputSchemeProvider
  & ProxiedActions<InputActions>
  & { sources: (tracks: Track[]) => Source[] };

export type InputSchemeProvider = { SCHEME: string };

export type ResolvedUri = undefined | ResolveUriAsUrl | ResolveUriAsStream;

export type ResolveUriAsUrl = {
  expiresAt: number;
  url: string;
};

export type ResolveUriAsStream = {
  expiresAt: number;
  mimeType: string;
  stream: ReadableStream;

  /** Total duration in seconds. */
  duration?: number;
};

export type Source = { label: string; uri: string };
````


### Metadata

Various ways to fetch metadata for tracks. Must adhere to API:

```ts
import type { Track } from "~/definitions/types.d.ts";

export type Actions = {
  patch(track: Track): Promise<Track>;
};
```


### Output

Output components hold all the user data: facets, playlist items, settings and tracks. They all have the same surface API. Data is exposed via a signal getter called `collection`, one for each kind of data. These components are also responsible loading and saving that data. The data may be encoded, each component decides for its own what the type of the data is. The subcategory states what that type is:

- `polymorphic`: The data can be of multiple types.
- `raw`: The data is not encoded at all, it is of the same type stated in the definitions (lexicons).
- `bytes`: Encoded as `Uint8Array`.


### Supplement

All kinds of additional components, not really required to make a working audio player application. Scrobbling for example.


### Transformer

Transformers are also intermediate components like configurators, with the same API requirements. They don't take DOM elements as children however, instead they serve to be used as a chain. Output transformers have a `output-selector` which chains the transformer to another component.
