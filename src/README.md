# Elm code

Elm directories:
- src/Core
- src/Library

`UI` is the Elm application that'll be executed on the main thread (ie. the UI thread) and `Brain` is the Elm application that'll live inside a web worker. `UI` will be the main application and `Brain` does the heavy lifting. The code shared between these two applications lives in `Library`. The library also contains the more "generic" code that's not necessarily tied to one or the other. Additionally you have `Themes` which is a layer on top of the UI code.



## Emoji Comments

```
⛩    Gates (Flags & Ports)
🌳    Types & Initial State
📣    Updates
📰    Subscriptions
🗺    Views

🧠    Brain
🛠️    Functions
🖼    Styles
㊙️    Secret
```
