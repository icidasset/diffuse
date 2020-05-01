> A music player that connects to your cloud &amp; distributed storage

[Return to the application](../)



## What makes it different?

Diffuse is a decentralized music player consisting out of two main parts. One part is the music and the other is your data <small>(eg. playlists)</small>, both of which are in locations of your choice. Meaning that there's no central server for Diffuse, all of the processing happens on your device and all the data is in your control. You can use the [web version](https://diffuse.sh), the [native version](https://github.com/icidasset/diffuse/releases) or host it yourself by downloading the pre-built packages from [Github](https://github.com/icidasset/diffuse).


### Music layer

This layer connects to the services on which your music is stored, no data is written to these services. You can combine all of the following:

- [Amazon S3](https://aws.amazon.com/s3/)
- [Azure Blob Storage](https://azure.microsoft.com/en-us/services/storage/blobs/)
- [Azure File Storage](https://azure.microsoft.com/en-us/services/storage/files/)
- [Dropbox](https://dropbox.com/)
- [Google Drive](https://drive.google.com/)
- [IPFS](https://ipfs.io/) <small>(supports DNSLink & IPNS)</small>
- [WebDAV](https://en.wikipedia.org/wiki/WebDAV)


### User layer

This layer will use a single service on which to store your data. Your data being your settings, favourites, playlists, etc. You can choose between these services:

- [Blockstack](https://blockstack.org/)
- [Dropbox](https://www.dropbox.com/)
- [IndexedDB](https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API) <small>(Browser)</small>
- [IPFS](https://ipfs.io/) <small>(using MFS)</small>
- [RemoteStorage](https://remotestorage.io/)
- [Textile](https://github.com/textileio/go-textile)



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

<small><em>Note, support may vary depending on your <a href="https://developer.mozilla.org/en-US/docs/Web/Media/Formats/Containers#Index_of_media_container_formats_file_types">browser</a>.</em></small>


<div id="CORS" />

### CORS

There's only one thing you need to do yourself so that the service you chose will work with the application, and that's setting up [CORS](https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS) (Cross-Origin Resource Sharing). Here are the instructions you'll need for each service:

<div id="CORS__S3" />

#### Amazon S3

You can find the CORS configuration editor under the "Permissions" tab, on the S3 AWS Console.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<CORSConfiguration xmlns="http://s3.amazonaws.com/doc/2006-03-01/">
<CORSRule>
    <AllowedOrigin>*</AllowedOrigin>
    <AllowedMethod>GET</AllowedMethod>
    <AllowedMethod>HEAD</AllowedMethod>
    <MaxAgeSeconds>31536000</MaxAgeSeconds>
    <ExposeHeader>Content-Length</ExposeHeader>
    <ExposeHeader>Content-Type</ExposeHeader>
    <AllowedHeader>Range</AllowedHeader>
</CORSRule>
</CORSConfiguration>
```

<div id="CORS__BTFS" />

#### BTFS

Add the domain of the app, with the protocol, to the __list of allowed origins__.  

```shell
btfs config --json API.HTTPHeaders.Access-Control-Allow-Origin '["https://diffuse.sh", "http://diffuse.sh.ipns.localhost:8080", "http://127.0.0.1:44999"]'
```

You can also make this change in the Web UI, you'll find it under "Settings → BTFS Config".

```javascript
{
  "API": {
    "HTTPHeaders": {
      "Access-Control-Allow-Origin": [
        "https://diffuse.sh",                       // 🎵 Default
        "http://diffuse.sh.ipns.localhost:8080",    // IPNS
        "http://127.0.0.1:44999"                    // Electron app
      ]
    }
  }
}
```

<div id="CORS__Dropbox" />

#### Dropbox

_Not necessary._

<div id="CORS__Google-Drive" />

#### Google Drive

_Not necessary._

<div id="CORS__IPFS" />

#### IPFS

Add the domain of the app, with the protocol, to the __list of allowed origins__.  

```shell
ipfs config --json API.HTTPHeaders.Access-Control-Allow-Origin '["https://diffuse.sh", "http://diffuse.sh.ipns.localhost:8080", "http://127.0.0.1:44999"]'
```

You can also make this change in the Web UI, you'll find it under "Settings → IPFS Config".

```javascript
{
  "API": {
    "HTTPHeaders": {
      "Access-Control-Allow-Origin": [
        "https://diffuse.sh",                       // 🎵 Default
        "http://diffuse.sh.ipns.localhost:8080",    // IPNS through IPFS Companion
        "http://127.0.0.1:44999"                    // Electron app
      ]
    }
  }
}
```

<div id="CORS__Azure" />

#### Microsoft Azure Storage

You can find the CORS configuration under the "Settings -> CORS".  
Then fill in the following in the input boxes (left to right):

```
ALLOWED ORIGINS       *
ALLOWED METHODS       GET, HEAD
ALLOWED HEADERS       Range
EXPOSED HEADERS       Content-Length, Content-Range
MAX AGE               0
```

<div id="CORS__Textile" />

#### Textile

Add the domain of the app, with the protocol, to the __list of allowed origins__ in the configuration.

```json
{
  "API": {
    "HTTPHeaders": {
      "Access-Control-Allow-Origin": [
        "https://diffuse.sh",
        "http://127.0.0.1:8080",
        "http://127.0.0.1:44999"
      ]
    }
  }
}
```

<div id="CORS__WebDAV" />

#### WebDAV

__Depends on your WebDAV server.__  
Example setup for Henrique Dias's [WebDAV server](https://github.com/hacdias/webdav):

```yaml
cors:
  enabled: true
  credentials: true

  allowed_headers:
    - Authorization
    - Content-Type
    - Depth
    - Range
  allowed_methods:
    - GET
    - HEAD
    - PROPFIND
  allowed_hosts:
    - https://diffuse.sh
    - http://127.0.0.1:44999
  exposed_headers:
    - Content-Length
    - Content-Type
```



<div id="UI" />

## UI

There are a few "hidden" features:

- **Tracks have a context menu** which can be opened by either right clicking,
  or holding it (ie. a long tap). Use the ALT key whilst right clicking
  on a track to show the alternative track-context menu with more options.
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
# Show me every track where the title, artist or album contains the term 'Parkway' and the term 'Drive'. Terms are separated by spaces (eg. "Killing with a smile" has four terms).
Parkway Drive

# Show me every track of which the artist's name contains 'park'.
artist:park*

# Show me every track from Parkway Drive's "Deep Blue" album.
artist:Parkway Drive album:Deep Blue

# Show me every track from Parkway Drive but not their "Atlas" album.
artist:Parkway Drive - album:Atlas
```

### Keyboard

The app should be usable with only the keyboard, there are various keyboard shortcuts:

```
P - Play / Pause
S - Toggle Shuffle
R - Toggle Repeat
N - Scroll to currently-playing track

Left / Right - Previous / Next
Up   / Down  - Seek forwards / Seek backwards
```



<div id="QA" />

## Q&A

### I used version one, where's my data?

There's a small link, or button if you will, on the "Settings -> Import / Export"
page that will allow you to import data from version 1 of the app. Note that this
will need to reflect the authentication/storage method you chose in version 1.
