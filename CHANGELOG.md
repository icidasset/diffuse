# Changelog

## 2.1.0

- **Added ability to cache an entire playlist** (for offline usage)
- **Automatically prepend `_dnslink.` when using a domain name with an IPFS source**
- Explain more things in the UI and on the about page
- Improve onboarding
- Show time and duration of the current track after hovering over the progress bar for a while
- Slightly improved tap/click events on tracks
- Smaller javascript files, ie. improved load tim

## 2.0.0

- **Ability to store tracks in your browser cache (ie. play offline)**
- **Adds support for IPFS DNSLink & IPNS (for sources)**
- **Adds support for more file formats** (thanks to [music-metadata-browser](https://github.com/Borewit/music-metadata-browser))
- **Adds support for storing application data on [Dropbox](https://dropbox.com/)**
- **Adds support for storing application data on [IPFS](https://ipfs.io/)**
- **Adds support for storing application data on [Textile](https://github.com/textileio/go-textile)**
- **Better mobile experience (PWA)**
- **Better search**
- **Data encryption**
- **Group by processing date (ie. added-to-collection date), track year, directory, etc.**
- **Preloads next track on every queue change**
- **Remembers the playback progress on long audio files (can be disabled)**
- **WebDAV support in the browser**
- Ability to stop processing
- Added 'Azure File Storage' service back
- Added, and removed, some background images (21 total now)
- Data is saved more efficiently and faster
- Data is saved in multiple files instead of one big file (each bit is in its respective file, eg. sources -> sources.json)
- Improved accessibility (eg. properly navigate between buttons/forms with keyboard)
- Improved audio streaming (better handling of edge cases, eg. connection drops)
- Improved drag & drop
- Improved error messages (eg. through detailed notifications)
- Improved performance
- Improved README (how to host yourself, etc.)
- Improved UI
- New default background and accent color (softer and warmer colors)
- Option to hide duplicate tracks
- Removed the "authenticating" wording (eg. "Store data on ..." instead of "Authenticate with ...")
- Select background-image using thumbnail grid
- Simpler setup (easier to build)
- Support for [Media Keys](https://github.com/borismus/keysocket) in the browser and the [Media Session API](https://developer.mozilla.org/en-US/docs/Web/API/Media_Session_API)
- Uses hash/fragment based routing for easier hosting
