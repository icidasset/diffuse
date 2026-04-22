import { walk } from "@std/fs/walk";

const srcDir = new URL("../", import.meta.url).pathname;
const sources = {};

for await (const entry of walk(srcDir + "components", { match: [/element\.js$/] })) {
  const content = await Deno.readTextFile(entry.path);
  const key = entry.path.slice(srcDir.length);
  sources[key] = content;
}

export default {
  sources,

  artwork: [
    { url: "components/artwork/audio-metadata/element.js", title: "Audio Metadata", desc: "Extracts embedded artwork from audio files using the music-metadata library." },
    { url: "components/artwork/input/element.js", title: "Input", desc: "Fetches artwork by delegating to the configured input element's artwork method." },
    { url: "components/artwork/last.fm/element.js", title: "Last.fm", desc: "Fetches cover art from the Last.fm API using track artist and album tags." },
    { url: "components/artwork/musicbrainz/element.js", title: "MusicBrainz", desc: "Fetches cover art from MusicBrainz and the Cover Art Archive using track artist and album tags." },
  ],

  configurators: [
    { url: "components/configurator/artwork/element.js", title: "Artwork", desc: "Takes artwork components as children and tries each in sequence, returning the first non-null result." },
    { url: "components/configurator/input/element.js", title: "Input", desc: "Allows for multiple inputs to be used at once." },
    { url: "components/configurator/metadata/element.js", title: "Metadata", desc: "Takes metadata components as children and chains their patches in sequence." },
    { url: "components/configurator/output/element.js", title: "Output", desc: "Enables the user to configure a specific output. If no default output is set, it creates a temporary session by storing everything in memory." },
    { url: "components/configurator/scrobbles/element.js", title: "Scrobbles", desc: "Configure multiple scrobblers (music trackers)." },
  ],

  engines: [
    { url: "components/engine/audio/element.js", title: "Audio", desc: "Plays audio through audio elements." },
    { url: "components/engine/queue/element.js", title: "Queue", desc: "A queue for tracks." },
    { url: "components/engine/repeat-shuffle/element.js", title: "Repeat & Shuffle", desc: "Signals synced with local storage (classified by group) that decide if audio should be repeated and if the queue should be shuffled when filling it." },
    { url: "components/engine/scope/element.js", title: "Scope", desc: "Signals that could influence the scope of a set of tracks." },
  ],

  input: [
    { url: "components/input/dropbox/element.js", title: "Dropbox", desc: "Audio files from Dropbox, using the Dropbox v2 HTTP API." },
    { url: "components/input/ephemeral-cache/element.js", title: "Ephemeral Cache", desc: "Wraps another input and caches its track listing in memory for the duration of the session." },
    { url: "components/input/https/element.js", title: "HTTPS", desc: "HTTPS URLs to audio files or streams." },
    { url: "components/input/icecast/element.js", title: "Icecast", desc: "Icecast internet radio streams. Fetches ICY metadata to populate track information." },
    { url: "components/input/local/element.js", title: "Local", desc: "Audio files or directories from your local device, using the browser's File System Access API." },
    { url: "components/input/opensubsonic/element.js", title: "Opensubsonic", desc: "Add any (open)subsonic server." },
    { url: "components/input/s3/element.js", title: "S3", desc: "AWS S3 and services that provide the same surface API such as Cloudflare R2." },
    { url: "components/input/webdav/element.js", title: "WebDAV", desc: "Add any WebDAV server." },
  ],

  metadata: [
    { url: "components/metadata/audio-file/element.js", title: "Audio File", desc: "Extracts tags and audio stats from audio files using the music-metadata library." },
  ],

  orchestrators: [
    { url: "components/orchestrator/artwork/element.js", title: "Artwork", desc: "Fetches cover art for a given set of tracks, stored locally in indexedDB. Uses the artwork configurator to try each configured source in sequence." },
    { url: "components/orchestrator/auto-queue/element.js", title: "Automatic Queue", desc: "Fill the queue automatically with non-manual items (shuffled or regular, based on repeat-shuffle engine)." },
    { url: "components/orchestrator/controller/element.js", title: "Controller", desc: "Provides commonly used computed signals derived from the audio engine, queue engine, and output. Exposes currentTrack(), isPlaying(), and references to the underlying engines." },
    { url: "components/orchestrator/cover-groups/element.js", title: "Cover Groups", desc: "Groups tracks by cover art to form collections." },
    { url: "components/orchestrator/favourites/element.js", title: "Favourites", desc: "Mark tracks as favourites. Automatically creates an unordered 'Favourites' playlist." },
    { url: "components/orchestrator/media-session/element.js", title: "Media Session", desc: "Keeps the browser/os media session in sync with queue and audio state. Adds handlers for previous, next, seek to, etc." },
    { url: "components/orchestrator/output/element.js", title: "Output", desc: "A default output configuration. Contains all the outputs provided here along with the relevant transformers." },
    { url: "components/orchestrator/path-collections/element.js", title: "Path Collections", desc: "Wraps an output element to generate ephemeral playlists based on the first path segment of each track's URI. Ephemeral items are excluded from storage." },
    { url: "components/orchestrator/process-tracks/element.js", title: "Process Inputs Into Tracks", desc: "Whenever the cached tracks are initially loaded through the passed output element it will list tracks by using the passed input element. Afterwards it loops over all tracks and checks if metadata needs to be fetched. If anything has changed, it'll pass the results to the output element." },
    { url: "components/orchestrator/queue-audio/element.js", title: "Queue ⭤ Audio", desc: "Connects the given queue engine to the given audio engine." },
    { url: "components/orchestrator/scoped-tracks/element.js", title: "Scoped Tracks", desc: "Watches the given output's tracks collection and runs them through a built-in search index. Can perform a search and other ways to reduce the scope of tracks based on the given scope engine. Provides a tracks signal similar to output.tracks.collection." },
    { url: "components/orchestrator/scrobble-audio/element.js", title: "Scrobble ⭤ Audio", desc: "Connects the audio engine with a scrobbler element. Calls nowPlaying when a track starts playing and scrobble once the user has listened long enough." },
    { url: "components/orchestrator/sources/element.js", title: "Sources", desc: "Monitor tracks from the given output to form a list of sources based on the input's sources return value." },
  ],

  output: [
    { url: "components/output/polymorphic/indexed-db/element.js", title: "Polymorphic / IndexedDB", desc: "Stores output into the local indexedDB. Supports any type of data that indexedDB supports." },
    { url: "components/output/bytes/s3/element.js", title: "Bytes / S3", desc: "Store output data on AWS S3 or compatible services such as Cloudflare R2." },
    { url: "components/output/raw/atproto/element.js", title: "Raw / AT Protocol", desc: "Store your user data on the storage associated with your ATProtocol identity. Data is lexicon shaped by default so this element takes in that data directly without any transformations." },
  ],

  supplements: [
    { url: "components/supplement/last.fm/element.js", title: "Last.fm Scrobbler", desc: "Scrobbles track plays to Last.fm." },
    { url: "components/supplement/listenbrainz/element.js", title: "ListenBrainz Scrobbler", desc: "Scrobbles track plays to ListenBrainz.", todo: true },
    { url: "components/supplement/rocksky/element.js", title: "Rocksky Scrobbler", desc: "Scrobbles track plays to Rocksky.", todo: true },
  ],

  transformers: [
    { url: "components/transformer/output/bytes/automerge/element.js", title: "Output / Bytes / Automerge", desc: "Translate data to and from an Automerge CRDT.", todo: true },
    { url: "components/transformer/output/bytes/dasl-sync/element.js", title: "Output / Bytes / DASL Sync", desc: "Syncs data between local and remote using CID-based diffing and performs union merges with tombstone tracking when both sides have diverged." },
    { url: "components/transformer/output/bytes/json/element.js", title: "Output / Bytes / JSON", desc: "Raw data schema output to and from JSON Uint8Array." },
    { url: "components/transformer/output/raw/atproto-sync/element.js", title: "Output / Raw / AT Protocol Sync", desc: "Wraps an AT Protocol output with a local IndexedDB cache. Uses the repo revision to skip unnecessary fetches and performs union merges with tombstone tracking when both local and remote have diverged." },
    { url: "components/transformer/output/refiner/default/element.js", title: "Output / Refiner / Default", desc: "Removes output state that is not meant to be saved to storage, such as ephemeral tracks. Ideally part of every theme." },
    { url: "components/transformer/output/refiner/initial-contents/element.js", title: "Output / Refiner / Initial Contents", desc: "Sets the initial contents for an output on first load." },
    { url: "components/transformer/output/refiner/passkey-encryption/element.js", title: "Output / Refiner / Track URI Passkey", desc: "Encrypts track URIs using a passkey-derived PRF key. On read, decrypts encrypted:// URIs transparently; on write, re-encrypts all URIs before passing downstream." },
    { url: "components/transformer/output/string/json/element.js", title: "Output / String / JSON", desc: "Raw data schema output to and from JSON UTF8 string." },
  ],

  definitions: [
    { url: "definitions/output/collaboration.json", title: "Output / Collaboration", desc: "Represents a collaboration between multiple collaborators on a subject, such as a playlist." },
    { url: "definitions/output/facet.json", title: "Output / Facet", desc: "Facet pointer or HTML snippet." },
    { url: "definitions/output/playlistItem.json", title: "Output / Playlist Item", desc: "Represents a single item in a playlist. Tracks are matched based on the given criteria. A playlist is formed by grouping items by their playlist property." },
    { title: "Output / Progress", desc: "Used to track progress of (long) audio playback.", todo: true },
    { url: "definitions/output/track.json", title: "Output / Track", desc: "Represents audio that can be played, or a placeholder for a source of tracks. Contains a URI that will resolve to the audio." },
    { url: "definitions/output/trackBundle.json", title: "Output / Track Bundle", desc: "A bundle of tracks." },
  ],
};
