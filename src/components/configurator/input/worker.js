import * as URI from "uri-js";

import { groupTracksPerScheme } from "@common/index.js";
import { connectionsFromQuery } from "../common.js";
import { use } from "@common/worker.js";

/**
 * @import {Track} from "@definitions/types.d.ts";
 * @import {GroupConsult, InputActions as Actions} from "@components/input/types.d.ts"
 */

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

const connections = connectionsFromQuery(location);

/**
 * @param {string} scheme
 * @param {string} actionName
 */
function proxy(scheme, actionName) {
  const worker = connections[scheme];
  const proxyFn = use(actionName, worker);

  return proxyFn;
}

/**
 * @param {string} scheme
 */
function isSupportedScheme(scheme) {
  return !!connections[scheme];
}

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {Actions['consult']}
 */
export async function consult(fileUriOrScheme) {
  const scheme = fileUriOrScheme.includes(":")
    ? URI.parse(fileUriOrScheme).scheme || fileUriOrScheme
    : fileUriOrScheme;

  if (!isSupportedScheme(scheme)) {
    return { supported: false, reason: "Unsupported scheme" };
  }

  return await proxy(scheme, "consult")(fileUriOrScheme);
}

/**
 * @type {Actions['contextualize']}
 */
export async function contextualize(tracks) {
  const groups = groupTracks(tracks);
  const promises = Object.entries(groups).map(
    async ([scheme, tracksGroup]) => {
      if (!isSupportedScheme(scheme) || tracksGroup.length === 0) return;
      return await proxy(scheme, "contextualize")(tracksGroup);
    },
  );

  await Promise.all(promises);
}

/**
 * @type {Actions['groupConsult']}
 */
export async function groupConsult(tracks) {
  const groups = groupTracksPerScheme(tracks);

  /** @type {GroupConsult[]} */
  const consultations = await Promise.all(
    Object.keys(groups).map(async (scheme) => {
      if (!isSupportedScheme(scheme)) {
        return {
          [scheme]: {
            available: false,
            reason: "Unsupported scheme",
            tracks: groups[scheme] || [],
          },
        };
      }

      return await proxy(scheme, "groupConsult")(groups[scheme] || {});
    }),
  );

  return consultations.reduce((acc, c) => {
    return { ...acc, ...c };
  }, {});
}

/**
 * @type {Actions['list']}
 */
export async function list(cachedTracks = []) {
  const groups = await groupConsult(cachedTracks);

  Object.keys(connections).forEach((scheme) => {
    if (!groups[scheme]) groups[scheme] = { available: true, tracks: [] };
  });

  const promises = Object.entries(groups).map(
    async ([scheme, { available, tracks }]) => {
      if (!available) return tracks;
      if (!isSupportedScheme(scheme)) return tracks;
      return await proxy(scheme, "list")(tracks);
    },
  );

  const nested = await Promise.all(promises);
  const tracks = nested.flat(1);

  return tracks;
}

/**
 * @type {Actions['resolve']}
 */
export async function resolve(args) {
  const scheme = args.uri.split(":", 1)[0];
  if (!isSupportedScheme(scheme)) return undefined;

  try {
    return await proxy(scheme, "resolve")(args);
  } catch (err) {
    console.error(
      `[configurator/input] Resolve error for scheme '${scheme}'.`,
      err,
    );
  }
}

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////

/**
 * @param {Track[]} tracks
 */
function groupTracks(tracks) {
  const grouped = groupTracksPerScheme(
    tracks,
    Object.fromEntries(
      Object.entries(connections).map(([k, _v]) => {
        return [k, []];
      }),
    ),
  );

  return grouped;
}
