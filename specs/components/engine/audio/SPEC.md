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
- The volume of the audio must be able to be set and persisted across sessions. This value depends on the used `group`.
- The mime type must be set on the audio if provided.
- The element must support a `initial-progress` attribute which can be used to start playing audio from that particular point.
- Audio must be able to preloaded in the background.

## Implementation specific

- When using a HTML audio element, use a silent audio file data-uri as the `src` to cancel the loading of any data.
