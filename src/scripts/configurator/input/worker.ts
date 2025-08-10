import * as URI from "uri-js";

import type {
  Consult,
  ConsultGrouping,
  GroupConsult,
  InputWorkerTasks,
  Track,
} from "@applets/core/types";
import { groupTracksPerScheme, initialConnections, provide } from "@scripts/common";

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////
const actions = {
  consult,
  contextualize,
  groupConsult,
  list,
  resolve,
};

const { connections, tasks } = provide({
  actions,
  connections: initialConnections<InputWorkerTasks>(["file+local", "opensubsonic", "s3"]),
  tasks: actions,
});

export type Actions = typeof actions;
export type Tasks = typeof tasks;

function isSupportedScheme(scheme: string) {
  return !!connections[scheme];
}

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

async function consult(fileUriOrScheme: string): Promise<Consult> {
  const scheme = fileUriOrScheme.includes(":")
    ? URI.parse(fileUriOrScheme).scheme || fileUriOrScheme
    : fileUriOrScheme;

  if (!isSupportedScheme(scheme)) {
    return { supported: false, reason: "Unsupported scheme" };
  }

  const conn = await connections[scheme].promise;
  return conn.consult(fileUriOrScheme);
}

async function contextualize(tracks: Track[]) {
  const groups = groupTracks(tracks);
  const promises = Object.entries(groups).map(async ([scheme, tracksGroup]: [string, Track[]]) => {
    if (!isSupportedScheme(scheme) || tracksGroup.length === 0) return;
    const conn = await connections[scheme].promise;
    await conn.contextualize(tracksGroup);
  });

  await Promise.all(promises);
}

async function groupConsult(tracks: Track[]) {
  const groups = groupTracksPerScheme(tracks);

  const consultations: GroupConsult[] = await Promise.all(
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

      const conn = await connections[scheme].promise;
      const result = await conn.groupConsult(groups[scheme] || {});

      return result;
    }),
  );

  return consultations.reduce((acc: GroupConsult, c: GroupConsult) => {
    return { ...acc, ...c };
  }, {});
}

async function list(cachedTracks: Track[] = []) {
  const groups = await groupConsult(cachedTracks);

  Object.keys(connections).forEach((scheme) => {
    if (!groups[scheme]) groups[scheme] = { available: true, tracks: [] };
  });

  const promises = Object.entries(groups).map(
    async ([scheme, { available, tracks }]: [string, ConsultGrouping]) => {
      if (!available) return tracks;
      if (!isSupportedScheme(scheme)) return tracks;
      const conn = await connections[scheme].promise;
      return conn.list(tracks);
    },
  );

  const nested = await Promise.all(promises);
  const tracks = nested.flat(1);

  return tracks;
}

async function resolve(args: { method: string; uri: string }) {
  const scheme = args.uri.split(":", 1)[0];
  if (!isSupportedScheme(scheme)) return undefined;

  try {
    const conn = await connections[scheme].promise;
    return await conn.resolve(args);
  } catch (err) {
    console.error(`[configuration/input] Resolve error for scheme '${scheme}'.`, err);
  }
}

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////

function groupTracks(tracks: Track[]) {
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
