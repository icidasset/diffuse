import * as URI from "uri-js";

import { groupTracksPerScheme } from "@common/index.js";
import { ostiary, rpc, workerProxy } from "@common/worker.js";

/**
 * @import {Track} from "@definitions/types.d.ts";
 * @import {GroupConsult, InputActions} from "@components/input/types.d.ts"
 * @import {ActionsWithTunnel, ProxiedActions} from "@common/worker.d.ts"
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
 * @type {ActionsWithTunnel<InputActions>['contextualize']}
 */
export async function contextualize({ data, ports }) {
  const tracks = data;
  const groups = groupTracks(tracks, ports);
  const promises = Object.entries(groups).map(
    async ([scheme, tracksGroup]) => {
      const input = grabInput(scheme, ports);
      if (!input || tracksGroup.length === 0) return;
      return await input.contextualize(tracksGroup);
    },
  );

  await Promise.all(promises);
}

/**
 * @type {ActionsWithTunnel<InputActions>['groupConsult']}
 */
export async function groupConsult({ data, ports }) {
  const tracks = data;
  const groups = groupTracksPerScheme(tracks);

  /** @type {GroupConsult[]} */
  const consultations = await Promise.all(
    Object.keys(groups).map(async (scheme) => {
      const input = grabInput(scheme, ports);

      if (!input) {
        return {
          [scheme]: {
            available: false,
            reason: "Unsupported scheme",
            tracks: groups[scheme] ?? [],
          },
        };
      }

      return await input.groupConsult(groups[scheme] ?? {});
    }),
  );

  return consultations.reduce((acc, c) => {
    return { ...acc, ...c };
  }, {});
}

/**
 * @type {ActionsWithTunnel<InputActions>['list']}
 */
export async function list({ data, ports }) {
  const groups = await groupConsult({ data, ports });

  Object.keys(ports).forEach((scheme) => {
    if (!groups[scheme]) groups[scheme] = { available: true, tracks: [] };
  });

  const promises = Object.entries(groups).map(
    async ([scheme, { available, tracks }]) => {
      if (!available) return tracks;

      const input = grabInput(scheme, ports);
      if (!input) return tracks;
      return await input.list(tracks);
    },
  );

  const nested = await Promise.all(promises);
  const tracks = nested.flat(1);

  return tracks;
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
    contextualize,
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
