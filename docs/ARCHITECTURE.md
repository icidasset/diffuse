# Architecture

Diffuse provides a set of custom (DOM) elements (aka. web components) that can be combined into a working audio player or media browser living in a web view.


## Components / Elements

There are various categories of these elements, and may have subcategories. Each element lives in its own directory, containing the code, documentation and other assets associated with that element. The directory for the elements is `src/components/`, each subdirectory is a category which in turn may have one or more subcategories. If a directory has an `element.js` then that directory represents the element, not a subcategory.

A few examples:
- `src/components/engine/audio/element.js`: Category = engine, element = audio
- `src/components/output/polymorphic/indexed-db/element.js`: Category = output, subcategory = polymorphic, element = indexed-db
- `src/components/transformer/output/refiner/default/element.js`: Category = transformer, subcategory 1 = output, subcategory 2 = refiner, element = default

Most categories of components are independent, they do not rely on other components. There are a few exceptions:

- The category named 'orchestrator'. These are compositions of other components, meaning they consume other components. These have attributes ending with `-selector` which is a DOM selector that points at the component that is the dependency.
- The category named 'configurator'. These take other components as DOM children with the same actions/methods as the configurator itself. They serve to delegate or combine.

A lot of components will have a worker (located in `worker.js` besides the `element.js`) which is connected to the element using `this.workerProxy()` in the constructor. The proxy itself may be stored on the class instance, but it is usually not required.


## Definitions

`src/definitions/` are lexicons, JSON schemas that describe data in the system.


## Themes

Like orchestrator components, these are compositions of elements. Unlike orchestrators however, it doesn't compose by the use of selectors, instead we write the custom elements as HTML and use the DOM as the composition layer. Alternatively, custom elements can be created in Javascript and then added to the DOM from there.


## Other directories

- `src/common`: Common Javascript code shared by various components and/or themes.
- `src/styles`: Common CSS shared by themes, the index page or constituents (part of themes).
- `src/favicons`, `src/fonts`, `src/images` are binary assets for themes and the index page (`src/index.vto`)
- `src/_components` and `src/_includes` are templates used in `.vto` templates, again themes and index page.
