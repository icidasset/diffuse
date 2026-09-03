# Audio engine

This component is responsible for playing audio in the browser or web view. This component does not depend on any Diffuse-specific data structures.

## Requirements

- It should be able to play audio from a URL to a binary audio file, or an audio stream.
- When given an URL, it should not load the entire audio file into memory.
- Seeking should be enabled if possible (obviously not feasible for live streams).
- The element should operate in broadcast mode when a group attribute is present.
- Broadcasting the audio element allows other browser tabs to monitor its playback progress and state, volume, etc.
- When the broadcast leader tab gets closed, another tab will take the leader role and when that happens; if the audio was playing, the audio must start playing in the tab of the new leader.
- Consumers of this custom element must be able to tap into the code that allows for audio visualisations. Though it is not required to work simultaneously on all browser tabs.
- Audio is routed through the Web Audio API (`createMediaElementSource`) so consumers can insert their own processing (equalizer filters, analyzers for visualizations, etc) via the exposed `webAudio` graph.
- The volume of the audio must be able to be set and persisted across sessions. This value depends on the used `group`.
- The mime type must be set on the audio if provided.
- The element must support a `initial-progress` attribute which can be used to start playing audio from that particular point.
- Audio must be able to preloaded in the background.

## Implementation specific

- When using a HTML audio element, use a silent audio file data-uri as the `src` to cancel the loading of any data.

### Web Audio graph

Every `<audio>` element is routed through a single shared `AudioContext` using `createMediaElementSource`. All sources land on a single post-volume `input` (a `GainNode`), which is connected straight to the destination by default:

```
source → input → destination
```

The exposed `webAudio` graph gives consumers (`context`, `input`, `destination`) a stable place to insert their own processing:

- `input` is the post-volume tap point that every `<audio>` element feeds into. `input.gain` carries the master volume, so `HTMLMediaElement.volume` is kept at unity for routed elements.
- To apply DSP, a consumer disconnects the default `input → destination` edge and reconnects it through its own chain, ending at the destination — e.g. `input → biquadFilter → analyser → destination` for an equalizer/visualizer.
- `destination` is `context.destination`, the standard output used to complete any inserted chain.
- The `AudioContext` starts in the browser-suspended state; the engine unlocks it on the first user gesture and resumes it on `play()`.
- The context is created lazily (the first time an element is routed or `webAudio` is accessed) and closed when the engine is disconnected.
