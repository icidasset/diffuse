# Infrastructure

Elm directories:

- Applications/Brain
- Applications/UI
- Library

`UI` is the Elm application that'll be executed on the main thread (ie. the UI thread) and `Brain` is the Elm application that'll live inside a web worker. `UI` will be the main application and `Brain` does the heavy lifting. The code shared between these two applications lives in `Library`.
