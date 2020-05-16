# Changelog

## 2.5.3

- Deprecate Blockstack & Textile
- Only process the new source when adding a new source

## 2.5.2

- Fixes authentication issue with WebDAV in Firefox
- Fixes issue with seeking audio when paused (no longer tries to re-pause, which doesn't work)

## 2.5.1

- Fixes processing issue

## 2.5.0

- **Large overhaul of the core code**
- Improves service worker (now claims control immediately)
- Removes leading slashes from directory groups
- Fixes Google Drive support (they made some API changes)
- Fixes IPFS support regarding v0.5.x (user-storage layer)
- Fixes issue with Dropbox tracks preloading
- Fixes issue with search results not updating while processing sources
- Fixes issue with tracks being invisible

## 2.4.0

- **Adds Last.fm integration**
- Fixes Amazon S3 processing issue in Chrome (max call stack)
- Improves audio loading/unloading (doesn't get stuck loading anymore when spamming the next button)

## 2.3.1

- Fixes Firefox CSS issues

## 2.3.0

- Adds ability to download a playlist as a zip file
- Asks for confirmation when removing a playlist
- Fixes dark styles for add-to-playlist overlay
- Improves CORS information a little bit
- Improves interaction with notifications
- Improves playlist drag & drop (now moves selection instead of item under cursor)
- No longer scrolls track list to the top when moving things in a playlist or when processing tracks
- Shows the correct error message when the browser cannot play a certain type of audio
- Switches key bindings for arrow up and down

## 2.2.3

- Fixes processing issue
- Fixes search text color
- Improves search regarding parentheses
- Uses variable fonts (loads fonts faster)

## 2.2.2

- Fixes several buttons that weren't working anymore

## 2.2.1

- Adds support for Google Drive nested directories
- Fixes processing issues with Google Drive
- Fixes processing issues with WebDAV

## 2.2.0

- **Added dark mode**
- **Added support for BTFS (an IPFS fork)**
- Added ability to rename sources (ie. after creation)
- Added keyboard shortcuts for play/pause, toggle shuffle, etc.  
  See UI section on the about page for more info.
- Improved IPFS support as a music source (now uses paths instead of hashes)
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
