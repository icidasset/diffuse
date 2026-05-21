import * as IDB from "idb-keyval";
import * as URI from "fast-uri";
import * as Cid from "~/common/cid.js";

import {
  CACHE_KEY_PREFIX,
  SCHEME as CACHE_SCHEME,
} from "~/components/input/ephemeral-cache/constants.js";

import { groupTracksPerScheme, groupUrisPerScheme } from "~/common/utils.js";
import { ostiary, rpc, workerProxy } from "~/common/worker.js";

/**
 * @import {Track} from "~/definitions/types.d.ts";
 * @import {GroupConsult, InputActions} from "@specs/components/input/types.d.ts"
 * @import {ActionsWithTunnel, ProxiedActions} from "~/common/worker.d.ts"
 * @import {Actions} from "@specs/components/configurator/input/types.d.ts"
 */

////////////////////////////////////////////
// INPUT ACTIONS
////////////////////////////////////////////

/**
 * @type {ActionsWithTunnel<Actions>['artwork']}
 */
export async function artwork({ data, ports }) {
  const uri = data;
  const scheme = uri.split(":", 1)[0];
  const input = grabInput(scheme, ports);
  if (!input) return null;

  return await input.artwork(uri);
}

/**
 * @type {ActionsWithTunnel<Actions>['consult']}
 */
export async function consult({ data, ports }) {
  const fileUriOrScheme = data;
  const scheme = fileUriOrScheme.includes(":")
    ? URI.parse(fileUriOrScheme).scheme || fileUriOrScheme
    : fileUriOrScheme;

  const input = grabInput(scheme, ports);

  if (!input) {
    return { supported: false, reason: "Unsupported scheme" };
  }

  return await input.consult(fileUriOrScheme);
}

/**
 * @type {ActionsWithTunnel<Actions>['detach']}
 */
export async function detach({ data, ports }) {
  const currentTracks = data.tracks;
  const groups = groupTracks(currentTracks, ports);

  const promises = Object.entries(groups).map(
    async ([scheme, tracksGroup]) => {
      const input = grabInput(scheme, ports);
      if (!input || tracksGroup.length === 0) return tracksGroup;
      if (
        data.fileUriOrScheme.includes("://")
          ? data.fileUriOrScheme.startsWith(`${scheme}://`) === false
          : data.fileUriOrScheme !== scheme
      ) return tracksGroup;

      return await input.detach({
        fileUriOrScheme: data.fileUriOrScheme,
        tracks: tracksGroup,
      });
    },
  );

  const nested = await Promise.all(promises);
  const tracks = nested.flat(1);

  return tracks;
}

/**
 * @type {ActionsWithTunnel<Actions>['groupConsult']}
 */
export async function groupConsult({ data, ports }) {
  const uris = data;
  const groups = groupUrisPerScheme(uris);

  /** @type {GroupConsult[]} */
  const consultations = await Promise.all(
    Object.keys(groups).map(async (scheme) => {
      const input = grabInput(scheme, ports);

      if (!input) {
        return {
          [scheme]: {
            available: false,
            reason: "Unsupported scheme",
            scheme,
            uris: groups[scheme] ?? [],
          },
        };
      }

      return await input.groupConsult(groups[scheme] ?? []);
    }),
  );

  return consultations.reduce((acc, c) => {
    return Object.assign(acc, c);
  }, {});
}

/**
 * @type {ActionsWithTunnel<Actions>['list']}
 */
export async function list({ data, ports }) {
  const tracks = data;
  const uris = tracks.map((/** @type {Track} */ t) => t.uri);

  /** @type {Map<string, Track>} */
  const tracksByUri = new Map(
    tracks.map((/** @type {Track} */ t) => [t.uri, t]),
  );

  const groups = await groupConsult({ data: uris, ports });

  const promises = Object.values(groups).map(
    async ({ available, scheme, uris }) => {
      const groupTracks = uris
        .map((uri) => tracksByUri.get(uri))
        .filter((/** @type {Track | undefined} */ t) => t !== undefined);

      if (!available) return groupTracks;

      const input = grabInput(scheme, ports);
      if (!input) return groupTracks;
      return await input.list(groupTracks);
    },
  );

  const nested = await Promise.all(promises);
  return nested.flat(1);
}

/**
 * @type {ActionsWithTunnel<Actions>['resolve']}
 */
export async function resolve({ data, ports }) {
  const uri = data.uri;

  const scheme = uri.split(":", 1)[0];
  const input = grabInput(scheme, ports);
  if (!input) return undefined;

  return await input.resolve(data);
}

////////////////////////////////////////////
// ADDITIONAL ACTIONS
////////////////////////////////////////////

/**
 * @type {ActionsWithTunnel<Actions>['cache']}
 */
export async function cache({ data, ports }) {
  const uris = data;

  await Promise.all(uris.map(async (uri) => {
    if (await IDB.get(CACHE_KEY_PREFIX + uri) !== undefined) return;

    const resolved = await resolve({ data: { uri }, ports });
    if (!resolved || "stream" in resolved) return;

    const response = await fetch(resolved.url);
    if (!response.ok) return;

    await IDB.set(CACHE_KEY_PREFIX + uri, await response.blob());
  }));
}

/**
 * @type {ActionsWithTunnel<Actions>['listCached']}
 */
export async function listCached() {
  const keys = /** @type {string[]} */ (await IDB.keys());
  return keys
    .filter((k) => k.startsWith(CACHE_KEY_PREFIX))
    .map((k) => k.slice(CACHE_KEY_PREFIX.length));
}

/**
 * @type {ActionsWithTunnel<Actions>['removeFromCache']}
 */
export async function removeFromCache({ data }) {
  const uris = data;

  await Promise.all(uris.map((uri) => IDB.del(CACHE_KEY_PREFIX + uri)));
}

/**
 * @type {ActionsWithTunnel<Actions>['cacheBlob']}
 */
export async function cacheBlob({ data }) {
  const blob = data;
  const buffer = await blob.arrayBuffer();
  const bytes = new Uint8Array(buffer);
  const cid = await Cid.create(0x55, bytes);
  const uri = `${CACHE_SCHEME}://${cid}`;
  if (await IDB.get(CACHE_KEY_PREFIX + uri) === undefined) {
    await IDB.set(CACHE_KEY_PREFIX + uri, blob);
  }
  return uri;
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, {
    artwork,
    consult,
    detach,
    groupConsult,
    list,
    resolve,

    cache,
    cacheBlob,
    listCached,
    removeFromCache,
  });
});

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////

/**
 * @param {string} scheme
 * @param {Record<string, MessagePort>} ports
 * @returns {ProxiedActions<InputActions> | null}
 */
function grabInput(scheme, ports) {
  const port = ports[scheme];
  if (!port) return null;

  return workerProxy(() => {
    port.start();
    return port;
  });
}

/**
 * @param {Track[]} tracks
 * @param {Record<string, MessagePort>} ports
 */
function groupTracks(tracks, ports) {
  const grouped = groupTracksPerScheme(
    tracks,
    Object.fromEntries(
      Object.keys(ports).map((k) => {
        return [k, []];
      }),
    ),
  );

  return grouped;
}
