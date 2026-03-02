import * as URI from "fast-uri";

import { groupTracksPerScheme, groupUrisPerScheme } from "~/common/utils.js";
import { ostiary, rpc, workerProxy } from "~/common/worker.js";

/**
 * @import {Track} from "~/definitions/types.d.ts";
 * @import {GroupConsult, InputActions} from "~/components/input/types.d.ts"
 * @import {ActionsWithTunnel, ProxiedActions} from "~/common/worker.d.ts"
 */

////////////////////////////////////////////
// INPUT ACTIONS
////////////////////////////////////////////

/**
 * @type {ActionsWithTunnel<InputActions>['consult']}
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
 * @type {ActionsWithTunnel<InputActions>['detach']}
 */
export async function detach({ data, ports }) {
  const cachedTracks = data.tracks;
  const groups = groupTracks(cachedTracks, ports);

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
 * @type {ActionsWithTunnel<InputActions>['groupConsult']}
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
 * @type {ActionsWithTunnel<InputActions>['list']}
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
 * @type {ActionsWithTunnel<InputActions>['resolve']}
 */
export async function resolve({ data, ports }) {
  const uri = data.uri;
  const scheme = uri.split(":", 1)[0];
  const input = grabInput(scheme, ports);
  if (!input) return undefined;

  const result = await input.resolve(data);
  return result;
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
