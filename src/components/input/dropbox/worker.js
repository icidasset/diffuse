import * as TID from "@atcute/tid";
import { ostiary, rpc } from "~/common/worker.js";
import {
  detach as detachUtil,
  groupKey,
} from "~/components/input/common.js";
import {
  accountId,
  accountsFromTracks,
  buildURI,
  checkAccessCached,
  getTemporaryLink,
  groupTracksByAccount,
  groupUrisByAccount,
  listFiles,
  parseURI,
} from "./common.js";
import { SCHEME } from "./constants.js";

/**
 * @import { InputActions as Actions, ConsultGrouping } from "@specs/components/input/types.d.ts";
 * @import { Track } from "~/definitions/types.d.ts"
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {Actions['artwork']}
 */
export async function artwork(_uri) {
  return null;
}

/**
 * @type {Actions['consult']}
 */
export async function consult(fileUriOrScheme) {
  if (!fileUriOrScheme.includes(":")) {
    return { supported: true, consult: "undetermined" };
  }

  const parsed = parseURI(fileUriOrScheme);
  if (!parsed) return { supported: true, consult: "undetermined" };

  const accessible = await checkAccessCached(parsed.accessToken);
  return { supported: true, consult: accessible };
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

      const id = accountId(result);
      const groups = groupTracksByAccount(tracks);
      delete groups[id];

      return Object.values(groups).map((g) => g.tracks).flat(1);
    },
  });
}

/**
 * @type {Actions['groupConsult']}
 */
export async function groupConsult(uris) {
  const groups = groupUrisByAccount(uris);

  const promises = Object.entries(groups).map(
    async ([id, { account, uris }]) => {
      const available = await checkAccessCached(account.accessToken);

      /** @type {ConsultGrouping} */
      const grouping = available
        ? { available, scheme: SCHEME, uris }
        : { available, reason: "Dropbox access denied", scheme: SCHEME, uris };

      return { key: groupKey(SCHEME, id), grouping };
    },
  );

  const entries = (await Promise.all(promises)).map((e) => [e.key, e.grouping]);
  return Object.fromEntries(entries);
}

/**
 * @type {Actions['list']}
 */
export async function list(cachedTracks = []) {
  const accounts = accountsFromTracks(cachedTracks);

  /** @type {Record<string, Record<string, Track>>} */
  const cache = {};

  cachedTracks.forEach((t) => {
    const parsed = parseURI(t.uri);
    if (!parsed || t.kind === "placeholder") return;

    const id = accountId(parsed);
    if (!cache[id]) cache[id] = {};
    cache[id][parsed.path] = t;
  });

  const promises = Object.values(accounts).map(async (account) => {
    const id = accountId(account);
    const files = await listFiles(account.accessToken, account.directoryPath);

    if (!files) {
      const existing = cachedTracks.find((t) => {
        const p = parseURI(t.uri);
        return p && accountId(p) === id && t.kind === "placeholder";
      });

      const now = new Date().toISOString();

      return [/** @type {Track} */ ({
        $type: "sh.diffuse.output.track",
        id: existing?.id ?? TID.now(),
        createdAt: existing?.createdAt ?? now,
        updatedAt: now,
        kind: "placeholder",
        uri: buildURI(account),
      })];
    }

    if (!files.length) {
      const now = new Date().toISOString();

      return [/** @type {Track} */ ({
        $type: "sh.diffuse.output.track",
        id: TID.now(),
        createdAt: now,
        updatedAt: now,
        kind: "placeholder",
        uri: buildURI(account),
      })];
    }

    return files.map((file) => {
      const uri = buildURI(account, file.path_lower);
      const cached = cache[id]?.[file.path_lower];
      const now = new Date().toISOString();

      /** @type {Track} */
      const track = {
        $type: "sh.diffuse.output.track",
        id: cached?.id ?? TID.now(),
        createdAt: cached?.createdAt ?? now,
        updatedAt: cached?.updatedAt ?? now,
        stats: cached?.stats,
        tags: cached?.tags,
        uri,
      };

      return track;
    });
  });

  return (await Promise.all(promises)).flat(1);
}

/**
 * @type {Actions['resolve']}
 */
export async function resolve({ uri }) {
  const parsed = parseURI(uri);
  if (!parsed || parsed.path === "/") return undefined;

  const link = await getTemporaryLink(parsed.accessToken, parsed.path);
  if (!link) return undefined;

  // Dropbox temporary links expire after 4 hours
  const expiresAt = Math.round(Date.now() / 1000) + 4 * 60 * 60;
  return { url: link, expiresAt };
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
  });
});
