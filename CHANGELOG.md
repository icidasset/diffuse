# Changelog

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
- Added 'Azure File Storage' service back
- Added, and removed, some background images (21 total now)
- Data is saved more efficiently and faster
- Data is saved in multiple files instead of one big file (each bit is in its respective file, eg. sources -> sources.json)
- Improved accessibility (eg. properly navigate between buttons/forms with keyboard)
- Improved audio streaming (better handling of edge cases, eg. connection drops)
- Improved drag & drop
- Improved error messages (eg. through detailed notifications)
- Improved README (how to host yourself, etc.)
- Improved UI
- New default background and accent color (softer and warmer colors)
- Option to hide duplicate tracks
- Removed the "authenticating" wording (eg. "Store data on ..." instead of "Authenticate with ...")
- Select background-image using thumbnail grid
- Simpler setup (easier to build)
- Support for [Media Keys](https://github.com/borismus/keysocket) in the browser
- Uses hash/fragment based routing for easier hosting
