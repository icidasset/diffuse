# Components

These web components are custom elements (DOM elements) that serve as building blocks to make web software.

## Requirements

- Components should never rely on instances of other components, except if they live in the 'orchestrator' category. Orchestrator components can refer to other components using a DOM selector, this can be provided via an attribute ending with `-selector` as its name.
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

Various ways to fetch artwork. Must adhere to API:

```ts
export type Actions = {
  get(track: Track): Promise<Uint8Array | null>;
};
```
