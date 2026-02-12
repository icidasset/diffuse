import { ostiary, rpc } from "@common/worker.js";
import {
  detach as detachUtil,
  groupKeyHash,
} from "@components/input/common.js";

import { groupTracksByHost, parseURI } from "./common.js";
import { SCHEME } from "./constants.js";

/**
 * @import { InputActions as Actions, ConsultGrouping } from "@components/input/types.d.ts";
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
    return { supported: false, reason: "Invalid HTTPS URL" };
  }

  // Ping the URL to check if it's reachable
  try {
    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), 5000); // 5 second timeout

    const response = await fetch(parsed.url, {
      method: "HEAD",
      signal: controller.signal,
    });

    clearTimeout(timeoutId);
    return { supported: true, consult: response.ok };
  } catch (error) {
    return { supported: true, consult: false };
  }
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

      const did = result.host;
      const groups = groupTracksByHost(tracks);

      delete groups[did];

      return Object.values(groups).map((a) => a.tracks).flat(1);
    },
  });
}

/**
 * @type {Actions['groupConsult']}
 */
export async function groupConsult(tracks) {
  const groups = groupTracksByHost(tracks);

  const promises = Object.entries(groups).map(
    async ([_domainId, { host, tracks }]) => {
      // Pick one track to test reachability
      const testTrack = tracks[0];
      let available = false;

      if (testTrack) {
        try {
          const controller = new AbortController();
          const timeoutId = setTimeout(() => controller.abort(), 5000); // 5 second timeout

          const response = await fetch(testTrack.uri, {
            method: "HEAD",
            signal: controller.signal,
          });

          clearTimeout(timeoutId);
          available = response.ok;
        } catch {
          available = false;
        }
      }

      /** @type {ConsultGrouping} */
      const grouping = available
        ? { available, scheme: SCHEME, tracks }
        : { available, reason: "Host unreachable", scheme: SCHEME, tracks };

      return {
        key: await groupKeyHash(SCHEME, host),
        grouping,
      };
    },
  );

  const entries = (await Promise.all(promises)).map((
    entry,
  ) => [entry.key, entry.grouping]);

  return Object.fromEntries(entries);
}

/**
 * @type {Actions['list']}
 */
export async function list(cachedTracks = []) {
  // HTTPS input doesn't discover tracks automatically.
  // It only manages URLs that were added manually.
  // Just return the cached tracks that match our scheme.
  return cachedTracks.filter((track) => {
    const parsed = parseURI(track.uri);
    return !!parsed;
  });
}

/**
 * @type {Actions['resolve']}
 */
export async function resolve({ method, uri }) {
  const parsed = parseURI(uri);
  if (!parsed) return undefined;

  // HTTPS URLs don't need resolution - they're already accessible.
  // Just return the URL as-is with a far-future expiration.
  const expiresInSeconds = 60 * 60 * 24 * 365; // 1 year
  const expiresAtSeconds = Math.round(Date.now() / 1000) + expiresInSeconds;

  return {
    url: parsed.url,
    expiresAt: expiresAtSeconds,
  };
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  // Setup RPC

  rpc(context, {
    consult,
    detach,
    groupConsult,
    list,
    resolve,
  });
});
