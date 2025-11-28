import * as URI from "uri-js";

import { groupTracksPerScheme } from "@common/index.js";
import { ostiary, rpc, workerProxy } from "@common/worker.js";

/**
 * @import {Track} from "@definitions/types.d.ts";
 * @import {GroupConsult, InputActions} from "@components/input/types.d.ts"
 * @import {ProxiedActions} from "@common/worker.d.ts"
 * @import {AdditionalActions} from "./types.d.ts"
 */

/** @type {Record<string, ProxiedActions<InputActions>>} */
const inputs = {};

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {AdditionalActions["configure"]}
 */
export function configure({ ports }) {
  Object.keys(ports).forEach((key) => {
    inputs[key.toLowerCase()] = workerProxy(() => {
      const port = ports[key];
      port.start();
      return port;
    });
  });
}

////////////////////////////////////////////
// INPUT ACTIONS
////////////////////////////////////////////

/**
 * @type {InputActions['consult']}
 */
export async function consult(fileUriOrScheme) {
  const scheme = fileUriOrScheme.includes(":")
    ? URI.parse(fileUriOrScheme).scheme || fileUriOrScheme
    : fileUriOrScheme;

  const input = grabInput(scheme);

  if (!input) {
    return { supported: false, reason: "Unsupported scheme" };
  }

  return await input.consult(fileUriOrScheme);
}

/**
 * @type {InputActions['contextualize']}
 */
export async function contextualize(tracks) {
  const groups = groupTracks(tracks);
  const promises = Object.entries(groups).map(
    async ([scheme, tracksGroup]) => {
      const input = grabInput(scheme);
      if (!input || tracksGroup.length === 0) return;
      return await input.contextualize(tracksGroup);
    },
  );

  await Promise.all(promises);
}

/**
 * @type {InputActions['groupConsult']}
 */
export async function groupConsult(tracks) {
  const groups = groupTracksPerScheme(tracks);

  /** @type {GroupConsult[]} */
  const consultations = await Promise.all(
    Object.keys(groups).map(async (scheme) => {
      const input = grabInput(scheme);

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
 * @type {InputActions['list']}
 */
export async function list(cachedTracks = []) {
  const groups = await groupConsult(cachedTracks);

  Object.keys(inputs).forEach((scheme) => {
    if (!groups[scheme]) groups[scheme] = { available: true, tracks: [] };
  });

  const promises = Object.entries(groups).map(
    async ([scheme, { available, tracks }]) => {
      if (!available) return tracks;

      const input = grabInput(scheme);
      if (!input) return tracks;
      return await input.list(tracks);
    },
  );

  const nested = await Promise.all(promises);
  const tracks = nested.flat(1);

  return tracks;
}

/**
 * @type {InputActions['resolve']}
 */
export async function resolve(args) {
  const scheme = args.uri.split(":", 1)[0];
  const input = grabInput(scheme);
  if (!input) return undefined;

  try {
    return await input.resolve(args);
  } catch (err) {
    console.error(
      `[configurator/input] Resolve error for scheme '${scheme}'.`,
      err,
    );
  }
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

    // Additional
    configure,
  });
});

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////

/**
 * @param {string} scheme
 */
function grabInput(scheme) {
  return inputs[scheme.toLowerCase()];
}

/**
 * @param {Track[]} tracks
 */
function groupTracks(tracks) {
  const grouped = groupTracksPerScheme(
    tracks,
    Object.fromEntries(
      Object.keys(inputs).map((k) => {
        return [k, []];
      }),
    ),
  );

  return grouped;
}
