> A music player that connects to your cloud & distributed storage

[Return to the application](../)<br />
[CORS instructions](cors/)<br />
[Developers](dev/)<br />
[Privacy policy](../privacy.txt)



## What makes it different?

Diffuse is a decentralized music player consisting out of two main parts. One part is the music and the other is your data <small>(eg. playlists)</small>, both of which are in locations of your choice. Meaning that there's no central server for Diffuse, all of the processing happens on your device and all the data is in your control. You can use the [web version](https://diffuse.sh), the [native version](https://github.com/icidasset/diffuse/releases) or host it yourself by downloading the pre-built packages from [Github](https://github.com/icidasset/diffuse).


### Music layer

This layer connects to the services on which your music is stored, no data is written to these services. You can combine all of the following:

- [Amazon S3](https://aws.amazon.com/s3/)
- [Azure Blob Storage](https://azure.microsoft.com/en-us/services/storage/blobs/)
- [Azure File Storage](https://azure.microsoft.com/en-us/services/storage/files/)
- [Dropbox](https://dropbox.com/)
- [IPFS](https://ipfs.io/) <small>(supports DNSLink & IPNS)</small>
- [WebDAV](https://en.wikipedia.org/wiki/WebDAV)


### User layer

This (optional) layer will use a single service on which to store your data externally. Your data being your settings, favourites, playlists, etc. You can choose between these services:

- [Dropbox](https://www.dropbox.com/)
- [IPFS](https://ipfs.io/) <small>(using MFS)</small>
- [RemoteStorage](https://remotestorage.io/)



<div id="How" />

## How does it work?

Diffuse locates all the music files on the given services, extracts the metadata and then stores it via the previously-explained user layer.


### Supported File Formats

- MP3
- MP4/M4A
- FLAC
- OGG
- WAV
- WEBM

*<small>Note, support may vary depending on your <a href="https://developer.mozilla.org/en-US/docs/Web/Media/Formats/Containers#Index_of_media_container_formats_file_types">browser</a>.</small>*



<div id="UI" />

## UI

There are a few "hidden" features:

- **Tracks have a context menu** which can be opened by either right clicking,
  or holding it (ie. a long tap). Use the ALT key whilst right clicking
  on a track to show the alternative track-context menu with more specialized options.
- **You can reorder items** in the queue or a playlist with drag-and-drop.
  Select the item you want to move by tapping on it, then tap and hold to move it around.
- You can select multiple tracks using the SHIFT key and then add that selection
  to the queue or a playlist using the context menu.
- Click on the now-playing bit to scroll to that track.
- Double tap on a EQ setting to reset it to its default value.

### Playlists

To add something to a playlist, and create that playlist if it doesn't exist yet, you open the context menu of a track. To move tracks around in a playlist, first select the track, then drag it.

### Search

```shell
# Show me every track where the title, artist or album contains the term 'Parkway' and the term 'Drive'.
Parkway Drive

# Show me every track of which the artist's name starts with 'park'.
artist:park

# Show me every track from Parkway Drive of which the album starts with "Deep Blue".
artist:Parkway Drive album:Deep Blue

# Show me every track from Parkway Drive but not their "Atlas" album.
artist:Parkway Drive - album:Atlas
```

### Keyboard

The app should be usable with only the keyboard, there are various keyboard shortcuts:

```js
CTRL + K or CMD + K // Show command palette

CTRL + L // Select playlist using autocompletion
CTRL + N // Scroll to currently-playing track
CTRL + P // Play / Pause
CTRL + R // Toggle Repeat
CTRL + S // Toggle Shuffle

CTRL + [ or ] // Previous / Next
CTRL + { or } // Seek forwards / Seek backwards

Alternatively you can use the media-control keys,
if your browser supports it.

ESC // Close overlay, close context-menu, deselect album cover, etc.

CTRL + 1 // Tracks
CTRL + 2 // Playlists
CTRL + 3 // Queue
CTRL + 4 // EQ

CTRL + 8 // Sources
CTRL + 9 // Settings
```



<div id="QA" />

## Q&A

### I used version one, where's my data?

There's a small link, or button if you will, on the "Settings → Import / Export"
page that will allow you to import data from version 1 of the app. Note that this
will need to reflect the authentication/storage method you chose in version 1.



<div id="Misc" />

## 🪐

_Part of the <a href="https://ring.0data.app/#random" target="_blank">App Ring</a>_
