import * as _ArtworkAudioMetadata from "./artwork/audio-metadata/element.js"
import * as _ArtworkInput from "./artwork/input/element.js"
import * as _ArtworkLastFm from "./artwork/last.fm/element.js"
import * as _ArtworkMusicbrainz from "./artwork/musicbrainz/element.js"
import * as _ConfiguratorArtwork from "./configurator/artwork/element.js"
import * as _ConfiguratorInput from "./configurator/input/element.js"
import * as _ConfiguratorMetadata from "./configurator/metadata/element.js"
import * as _ConfiguratorOutput from "./configurator/output/element.js"
import * as _ConfiguratorScrobbles from "./configurator/scrobbles/element.js"
import * as _EngineAudio from "./engine/audio/element.js"
import * as _EngineQueue from "./engine/queue/element.js"
import * as _EngineRepeatShuffle from "./engine/repeat-shuffle/element.js"
import * as _EngineScope from "./engine/scope/element.js"
import * as _InputDropbox from "./input/dropbox/element.js"
import * as _InputEphemeralCache from "./input/ephemeral-cache/element.js"
import * as _InputHttps from "./input/https/element.js"
import * as _InputIcecast from "./input/icecast/element.js"
import * as _InputLocal from "./input/local/element.js"
import * as _InputOpensubsonic from "./input/opensubsonic/element.js"
import * as _InputS3 from "./input/s3/element.js"
import * as _InputWebdav from "./input/webdav/element.js"
import * as _MetadataAudioFile from "./metadata/audio-file/element.js"
import * as _OrchestratorArtwork from "./orchestrator/artwork/element.js"
import * as _OrchestratorAutoQueue from "./orchestrator/auto-queue/element.js"
import * as _OrchestratorController from "./orchestrator/controller/element.js"
import * as _OrchestratorCoverGroups from "./orchestrator/cover-groups/element.js"
import * as _OrchestratorFavourites from "./orchestrator/favourites/element.js"
import * as _OrchestratorMediaSession from "./orchestrator/media-session/element.js"
import * as _OrchestratorOffline from "./orchestrator/offline/element.js"
import * as _OrchestratorOutput from "./orchestrator/output/element.js"
import * as _OrchestratorPathCollections from "./orchestrator/path-collections/element.js"
import * as _OrchestratorProcessTracks from "./orchestrator/process-tracks/element.js"
import * as _OrchestratorQueueAudio from "./orchestrator/queue-audio/element.js"
import * as _OrchestratorScopedTracks from "./orchestrator/scoped-tracks/element.js"
import * as _OrchestratorScrobbleAudio from "./orchestrator/scrobble-audio/element.js"
import * as _OrchestratorSources from "./orchestrator/sources/element.js"
import * as _OutputBytesS3 from "./output/bytes/s3/element.js"
import * as _OutputPolymorphicIndexedDb from "./output/polymorphic/indexed-db/element.js"
import * as _OutputRawAtproto from "./output/raw/atproto/element.js"
import * as _SupplementLastFm from "./supplement/last.fm/element.js"
import * as _SupplementListenbrainz from "./supplement/listenbrainz/element.js"
import * as _SupplementRocksky from "./supplement/rocksky/element.js"
import * as _TransformerOutputBytesAutomerge from "./transformer/output/bytes/automerge/element.js"
import * as _TransformerOutputBytesDaslSync from "./transformer/output/bytes/dasl-sync/element.js"
import * as _TransformerOutputBytesJson from "./transformer/output/bytes/json/element.js"
import * as _TransformerOutputRawAtprotoSync from "./transformer/output/raw/atproto-sync/element.js"
import * as _TransformerOutputRefinerDefault from "./transformer/output/refiner/default/element.js"
import * as _TransformerOutputRefinerInitialContents from "./transformer/output/refiner/initial-contents/element.js"
import * as _TransformerOutputRefinerPasskeyEncryption from "./transformer/output/refiner/passkey-encryption/element.js"
import * as _TransformerOutputStringJson from "./transformer/output/string/json/element.js"

export const artwork = {
  audioMetadata: _ArtworkAudioMetadata,
  input: _ArtworkInput,
  lastFm: _ArtworkLastFm,
  musicbrainz: _ArtworkMusicbrainz,
}

export const configurator = {
  artwork: _ConfiguratorArtwork,
  input: _ConfiguratorInput,
  metadata: _ConfiguratorMetadata,
  output: _ConfiguratorOutput,
  scrobbles: _ConfiguratorScrobbles,
}

export const engine = {
  audio: _EngineAudio,
  queue: _EngineQueue,
  repeatShuffle: _EngineRepeatShuffle,
  scope: _EngineScope,
}

export const input = {
  dropbox: _InputDropbox,
  ephemeralCache: _InputEphemeralCache,
  https: _InputHttps,
  icecast: _InputIcecast,
  local: _InputLocal,
  opensubsonic: _InputOpensubsonic,
  s3: _InputS3,
  webdav: _InputWebdav,
}

export const metadata = {
  audioFile: _MetadataAudioFile,
}

export const orchestrator = {
  artwork: _OrchestratorArtwork,
  autoQueue: _OrchestratorAutoQueue,
  controller: _OrchestratorController,
  coverGroups: _OrchestratorCoverGroups,
  favourites: _OrchestratorFavourites,
  mediaSession: _OrchestratorMediaSession,
  offline: _OrchestratorOffline,
  output: _OrchestratorOutput,
  pathCollections: _OrchestratorPathCollections,
  processTracks: _OrchestratorProcessTracks,
  queueAudio: _OrchestratorQueueAudio,
  scopedTracks: _OrchestratorScopedTracks,
  scrobbleAudio: _OrchestratorScrobbleAudio,
  sources: _OrchestratorSources,
}

export const output = {
  bytes: {
    s3: _OutputBytesS3,
  },
  polymorphic: {
    indexedDb: _OutputPolymorphicIndexedDb,
  },
  raw: {
    atproto: _OutputRawAtproto,
  },
}

export const supplement = {
  lastFm: _SupplementLastFm,
  listenbrainz: _SupplementListenbrainz,
  rocksky: _SupplementRocksky,
}

export const transformer = {
  output: {
    bytes: {
      automerge: _TransformerOutputBytesAutomerge,
      daslSync: _TransformerOutputBytesDaslSync,
      json: _TransformerOutputBytesJson,
    },
    raw: {
      atprotoSync: _TransformerOutputRawAtprotoSync,
    },
    refiner: {
      default: _TransformerOutputRefinerDefault,
      initialContents: _TransformerOutputRefinerInitialContents,
      passkeyEncryption: _TransformerOutputRefinerPasskeyEncryption,
    },
    string: {
      json: _TransformerOutputStringJson,
    },
  },
}
