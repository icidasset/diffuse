# Audio engine

This component is responsible for playing audio in the browser or web view. This component does not depend on any Diffuse-specific data structures.

## Requirements

- It should be able to play audio from a URL to a binary audio file, or an audio stream.
- When given an URL, it should not load the entire audio file into memory.
- Seeking should be enabled if possible (obviously not feasible for live streams).
- The element should operate in broadcast mode when a group attribute is present.
- Broadcasting the audio element allows other browser tabs to monitor its playback progress and state, volume, etc.
- Consumers of this custom element must be able to tap into the code that allows for audio visualisations. Though it is not required to work simultaneously on all browser tabs.
