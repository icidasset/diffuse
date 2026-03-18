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


## Other directories

- `src/common`: Common Javascript code shared by various components and/or pages.
- `src/styles`: Common CSS shared by pages or facets.
- `src/favicons`, `src/fonts`, `src/images` are binary assets for facets and pages.
- `src/_components` and `src/_includes` are templates used in `.vto` templates.
