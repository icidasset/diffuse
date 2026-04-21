# Architecture

## Components / Elements

Diffuse provides a set of custom (DOM) elements (aka. web components) that can be combined into a working audio player or media browser living in a web view.

There are various categories of these elements, and may have subcategories. Each element lives in its own directory, containing the code, documentation and other assets associated with that element. The directory for the elements is `src/components/`, each subdirectory is a category which in turn may have one or more subcategories. If a directory has an `element.js` then that directory represents the element, not a subcategory.

These elements are available in the build as well, under the same `components` directory. These will be built as code-splitted bundled ESNext Javascript. They are listed on the `elements/` page with links to each Javascript file that defines that custom element.

A few examples:
- `src/components/engine/audio/element.js`: Category = engine, element = audio
- `src/components/output/polymorphic/indexed-db/element.js`: Category = output, subcategory = polymorphic, element = indexed-db
- `src/components/transformer/output/refiner/default/element.js`: Category = transformer, subcategory 1 = output, subcategory 2 = refiner, element = default

Most categories of components are independent, they do not rely on other components. There are a few exceptions:

- The category named 'orchestrator'. These are compositions of other components, meaning they consume other components. These have attributes ending with `-selector` which is a DOM selector that points at the component that is the dependency.
- The category named 'configurator'. These take other components as DOM children with the same actions/methods as the configurator itself. They serve to delegate or combine.

A lot of components will have a worker (located in `worker.js` besides the `element.js`) which is connected to the element using `this.workerProxy()` in the constructor. The proxy itself may be stored on the class instance, but it is usually not required.

Components share state with each other using signals (see `common/signal.js`) or using the worker. When using signals, do not expose the signal setter, unless it's absolutely necessary. The preferred format is `element.signalGetter()`



## Definitions

`src/definitions/` are atproto lexicons, JSON schemas that describe data in the system.



## Facets

These are pieces of HTML that represent either an "interface" or a "feature" (called "interactive" or "prelude" facets respectively). They are loaded using the facet loader that lives in the `l` directory.

Interface facets can be loaded from the default Diffuse set, the user's local data cache, an HTTPS URL or an [AT Protocol](https://atproto.com) URI.

Each time an interface facet is loaded, all enabled feature facets from the user's local data cache are loaded too beforehand. The default set of facets lives in the `facets` directory, and a list of these can be found in `_data/facets.json`.

Note that every interface facet MUST remove the loading animation using `foundation.ready()` otherwise the loading animation will stay visible, and ideally it would set the document title as well using `foundation.setup({ title })`.

The loader injects the HTML into a `<div id="container"></div>` element in the body. Facets don't have a doctype, a `<html>` element, or a `<head>` element.

Finally, note that the loader page configures a `<base>` element, every relative url becomes relative to the root of the Diffuse build. Every Diffuse page defines an import map which specifies `~` as the root.



## Foundation

This is a default configuration of the Diffuse elements. It is used throughout the default set of facets. It configures the elements to be part of the `facets` group, though this can be overriden using the `group` url query parameter.

A lot of the elements are written so that when the `group` attribute is set, it will create a broadcast channel to communicate within the group and/or create a shared worker.



## Other directories

- `src/common`: Common Javascript code shared by various components and/or pages.
- `src/styles`: Common CSS shared by pages or facets.
- `src/vendor`: Vendored Javascript to be used in facets.
- `src/oauth/callback`: Generic callback page to handle OAuth redirects.
- `src/favicons`, `src/fonts`, `src/images` are binary assets for facets and pages.
- `src/_components` and `src/_includes` are templates used in `.vto` templates. Part of the static Lume site.
