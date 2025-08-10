import type { URLTrack } from "webamp";
import Webamp from "webamp";

import type { GroupConsult, ManagedOutput, ResolvedUri, Track } from "@applets/core/types.d.ts";
import { applet, inputUrl, wait } from "@scripts/applet/common";

////////////////////////////////////////////
// 🗂️ Applets
////////////////////////////////////////////
const configurator = {
  input: applet("/configurator/input"),
  output: applet<ManagedOutput>("/configurator/output"),
};

const orchestrator = {
  queueAudio: applet("/orchestrator/queue-audio"),
  queueTracks: applet("/orchestrator/queue-tracks"),
  processTracks: applet("/orchestrator/process-tracks"),
};

////////////////////////////////////////////
// ⚡
////////////////////////////////////////////
const amp = new Webamp({
  initialTracks: [],
});

// Override
const loadFromUrl = amp.media.loadFromUrl.bind(amp.media);

async function loadOverride(uri: string, autoPlay: boolean) {
  const resp = await inputUrl(await configurator.input, uri);
  if (!resp) throw new Error("Failed to resolve URI");
  return await loadFromUrl(resp.url, autoPlay);
}

amp.media.loadFromUrl = loadOverride.bind(amp.media);

// Render
const ampNode = document.createElement("div");
ampNode.style = "height: 100vh; left: 0; position: absolute; top: 0; width: 100%; z-index: -1000;";
document.body.appendChild(ampNode);
amp.renderWhenReady(ampNode);

// Wait for tracks to load
configurator.output
  .then((output) => {
    output.ondata = loadAndInsert;
    return wait(output, (d) => d?.tracks.state === "loaded");
  })
  .then(async () => {
    await loadAndInsert();
  });

// Load & insert
let inserting = false;
let tracksCacheId: string | undefined = undefined;

async function loadAndInsert() {
  const output = await configurator.output;

  if (output.data.tracks.state !== "loaded") return;
  if (output.data.tracks.cacheId === tracksCacheId) return;
  if (inserting) return;

  inserting = true;
  tracksCacheId = output.data.tracks.cacheId;
  const tracks = await loadTracks();

  // TODO: This kinda messes up the UI,
  //       but at least the active audio doesn't stop playing.
  amp.store.dispatch({ type: "REMOVE_ALL_TRACKS" });

  // TODO: Webamp blows up if you add too much tracks
  amp.appendTracks(tracks.slice(0, 1000));

  const status = amp.getMediaStatus();
  if (status !== "PLAYING") amp.nextTrack();

  inserting = false;
}

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////
async function loadTracks(): Promise<URLTrack[]> {
  const input = await configurator.input;
  const output = await configurator.output;

  const groups = await input.sendAction<GroupConsult>(
    "groupConsult",
    output.data.tracks.collection,
    { timeoutDuration: 60000 * 5, worker: true },
  );

  // Available tracks
  let tracks: Track[] = [];

  Object.values(groups).forEach((value) => {
    if (value.available === false) return;
    tracks = tracks.concat(value.tracks);
  }, []);

  return tracks.map((track) => {
    const urlTrack: URLTrack = {
      url: track.uri,
      metaData: {
        title: track.tags?.title || "",
        artist: track.tags?.artist || "",
        album: track.tags?.album,
      },
      duration: track.stats?.duration,
    };

    return urlTrack;
  });
}
