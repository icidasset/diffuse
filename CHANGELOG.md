# Changelog

## 2.2.0

- Added keyboard shortcuts for play/pause, toggle shuffle, etc.  
  See UI section on the about page for more info.
- Added support for BTFS (an IPFS fork)
- Improved text readability (contrast, etc.)

## 2.1.2

- Fixed issue with WebDAV (the parser assumed tags in the form of `<D:` instead of allowing both upper and lower case)
- No longer listens to the `stalled` audio event (the behavior for this event is not consistent across browsers)
- Shows warning for IPFS MFS auth when using HTTPS

## 2.1.1

- Fixed issue with IPFS request timeout duration
- Fixed issue with Safari (ie. nested web workers)
- IPFS DNSLink lookups are done through the IPFS gateway instead of Cloudflare.

## 2.1.0

- **Added ability to cache an entire playlist** (for offline usage)
- **Automatically prepend `_dnslink.` when using a domain name with an IPFS source**
- Better color for the current-playing track
- Explain more things in the UI and on the about page
- Fixed issue with moving things in the queue
- Fixed issue with symbols in search
- Improve onboarding
- Replaced black favicon with a grey one
- Show processing progress on sources screen
- Show the amount of tracks you have
- Slightly improved tap/click events on tracks
- Smaller javascript files, ie. improved load time

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
