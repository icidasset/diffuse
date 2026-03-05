import { ostiary, rpc } from "~/common/worker.js";
import { detach as detachUtil, groupKey } from "~/components/input/common.js";

import {
  consultStreamCached,
  fetchMetadata,
  groupTracksByHost,
  groupUrisByHost,
  parseURI,
} from "./common.js";
import { SCHEME } from "./constants.js";

/**
 * @import { InputActions as Actions, ConsultGrouping } from "~/components/input/types.d.ts";
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {Actions['consult']}
 */
export async function consult(fileUriOrScheme) {
  if (!fileUriOrScheme.includes(":")) {
    return { supported: true, consult: "undetermined" };
  }

  const parsed = parseURI(fileUriOrScheme);
  if (!parsed) {
    return { supported: false, reason: "Invalid Icecast URI" };
  }

  const available = await consultStreamCached(fileUriOrScheme);
  return { supported: true, consult: available };
}

/**
 * @type {Actions['detach']}
 */
export async function detach(args) {
  return detachUtil({
    ...args,

    inputScheme: SCHEME,
    handleFileUri: ({ fileURI, tracks }) => {
      const result = parseURI(fileURI);
      if (!result) return tracks;

      const groups = groupTracksByHost(tracks);
      delete groups[result.host];

      return Object.values(groups).map((g) => g.tracks).flat(1);
    },
  });
}

/**
 * @type {Actions['groupConsult']}
 */
export async function groupConsult(uris) {
  const groups = groupUrisByHost(uris);

  const promises = Object.entries(groups).map(
    async ([_hostId, { host, uris }]) => {
      const testUri = uris[0];
      const available = testUri ? await consultStreamCached(testUri) : false;

      /** @type {ConsultGrouping} */
      const grouping = available
        ? { available, scheme: SCHEME, uris }
        : { available, reason: "Stream unreachable", scheme: SCHEME, uris };

      return {
        key: groupKey(SCHEME, host),
        grouping,
      };
    },
  );

  const entries = (await Promise.all(promises)).map((entry) => [
    entry.key,
    entry.grouping,
  ]);

  return Object.fromEntries(entries);
}

/**
 * @type {Actions['list']}
 */
export async function list(cachedTracks = []) {
  const refreshed = await Promise.all(
    cachedTracks.map(async (track) => {
      const parsed = parseURI(track.uri);
      if (!parsed) return track;

      const metadata = await fetchMetadata(parsed.httpsUrl);
      if (!metadata) return track;

      return {
        ...track,
        kind: /** @type {"stream"} */ ("stream"),
        tags: {
          ...track.tags,
          title: metadata.name ?? track.tags?.title,
          genres: metadata.genre ? [metadata.genre] : track.tags?.genres,
        },
        stats: {
          ...track.stats,
          // IcyMetadata.bitrate is in kbps; stats.bitrate is in bps
          bitrate: metadata.bitrate
            ? metadata.bitrate * 1000
            : track.stats?.bitrate,
        },
      };
    }),
  );

  return refreshed;
}

/**
 * @type {Actions['resolve']}
 */
export async function resolve({ uri }) {
  const parsed = parseURI(uri);
  if (!parsed) return undefined;

  const expiresInSeconds = 60 * 60 * 24 * 365; // 1 year
  const expiresAtSeconds = Math.round(Date.now() / 1000) + expiresInSeconds;

  return {
    url: parsed.httpsUrl,
    expiresAt: expiresAtSeconds,
  };
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, {
    consult,
    detach,
    groupConsult,
    list,
    resolve,
  });
});
