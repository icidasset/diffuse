import * as URI from "fast-uri";
import QS from "query-string";

import { cachedConsult, isAudioFile } from "~/components/input/common.js";
import { safeDecodeURIComponent } from "~/common/utils.js";
import { SCHEME } from "./constants.js";

/**
 * @import { Track } from "~/definitions/types.d.ts"
 */

/**
 * @typedef {{ accessToken: string; directoryPath: string }} Account
 */

////////////////////////////////////////////
// URI
////////////////////////////////////////////

/**
 * @param {Account} account
 * @param {string} [filePath]
 */
export function buildURI(account, filePath) {
  return URI.serialize({
    scheme: SCHEME,
    userinfo: encodeURIComponent(account.accessToken),
    host: "dropbox.com",
    path: filePath || "/",
    query: QS.stringify({ dir: account.directoryPath || "/" }),
  });
}

/**
 * @param {string} uriString
 * @returns {{ accessToken: string; path: string; directoryPath: string } | undefined}
 */
export function parseURI(uriString) {
  const uri = URI.parse(uriString);
  if (uri.scheme !== SCHEME) return undefined;
  if (!uri.userinfo) return undefined;

  const accessToken = decodeURIComponent(uri.userinfo);
  const path = safeDecodeURIComponent(uri.path || "/");
  const qs = QS.parse(uri.query || "");
  const directoryPath = typeof qs.dir === "string" ? safeDecodeURIComponent(qs.dir) : "/";

  return { accessToken, path, directoryPath };
}

////////////////////////////////////////////
// ACCOUNT HELPERS
////////////////////////////////////////////

/**
 * @param {Account} account
 */
export function accountId(account) {
  return `${account.accessToken}:${account.directoryPath}`;
}

/**
 * @param {Track[]} tracks
 * @returns {Record<string, Account>}
 */
export function accountsFromTracks(tracks) {
  /** @type {Record<string, Account>} */
  const acc = {};

  tracks.forEach((track) => {
    const parsed = parseURI(track.uri);
    if (!parsed) return;

    const id = accountId(parsed);
    if (acc[id]) return;

    acc[id] = { accessToken: parsed.accessToken, directoryPath: parsed.directoryPath };
  });

  return acc;
}

/**
 * @param {Track[]} tracks
 * @returns {Record<string, { account: Account; tracks: Track[] }>}
 */
export function groupTracksByAccount(tracks) {
  /** @type {Record<string, { account: Account; tracks: Track[] }>} */
  const acc = {};

  tracks.forEach((track) => {
    const parsed = parseURI(track.uri);
    if (!parsed) return;

    const id = accountId(parsed);

    if (acc[id]) {
      acc[id].tracks.push(track);
    } else {
      acc[id] = {
        account: { accessToken: parsed.accessToken, directoryPath: parsed.directoryPath },
        tracks: [track],
      };
    }
  });

  return acc;
}

/**
 * @param {string[]} uris
 * @returns {Record<string, { account: Account; uris: string[] }>}
 */
export function groupUrisByAccount(uris) {
  /** @type {Record<string, { account: Account; uris: string[] }>} */
  const acc = {};

  uris.forEach((uri) => {
    const parsed = parseURI(uri);
    if (!parsed) return;

    const id = accountId(parsed);

    if (acc[id]) {
      acc[id].uris.push(uri);
    } else {
      acc[id] = {
        account: { accessToken: parsed.accessToken, directoryPath: parsed.directoryPath },
        uris: [uri],
      };
    }
  });

  return acc;
}

////////////////////////////////////////////
// DROPBOX API
////////////////////////////////////////////

/**
 * @param {string} accessToken
 * @param {string} directoryPath
 * @returns {Promise<Array<{ name: string; path_lower: string }> | null>}
 */
export async function listFiles(accessToken, directoryPath) {
  const apiPath = directoryPath === "/" ? "" : directoryPath;
  const headers = {
    "Authorization": `Bearer ${accessToken}`,
    "Content-Type": "application/json",
  };

  /** @type {Array<{ name: string; path_lower: string }>} */
  const entries = [];
  let cursor = /** @type {string | null} */ (null);
  let hasMore = true;

  while (hasMore) {
    const url = cursor
      ? "https://api.dropboxapi.com/2/files/list_folder/continue"
      : "https://api.dropboxapi.com/2/files/list_folder";

    const body = cursor
      ? JSON.stringify({ cursor })
      : JSON.stringify({ path: apiPath, recursive: true, limit: 2000 });

    const resp = await fetch(url, { method: "POST", headers, body });
    if (!resp.ok) return null;

    /** @type {{ entries: Array<{ ".tag": string; name: string; path_lower: string }>; has_more: boolean; cursor: string }} */
    const data = await resp.json();

    for (const entry of data.entries) {
      if (entry[".tag"] === "file" && isAudioFile(entry.name)) {
        entries.push({ name: entry.name, path_lower: entry.path_lower });
      }
    }

    hasMore = data.has_more;
    cursor = data.cursor;
  }

  return entries;
}

/**
 * @param {string} accessToken
 * @param {string} filePath
 * @returns {Promise<string | null>}
 */
export async function getTemporaryLink(accessToken, filePath) {
  const resp = await fetch(
    "https://api.dropboxapi.com/2/files/get_temporary_link",
    {
      method: "POST",
      headers: {
        "Authorization": `Bearer ${accessToken}`,
        "Content-Type": "application/json",
      },
      body: JSON.stringify({ path: filePath }),
    },
  );

  if (!resp.ok) return null;

  /** @type {{ link: string }} */
  const data = await resp.json();
  return data.link ?? null;
}

/**
 * @param {string} accessToken
 * @returns {Promise<boolean>}
 */
export async function checkAccess(accessToken) {
  const resp = await fetch(
    "https://api.dropboxapi.com/2/users/get_current_account",
    {
      method: "POST",
      headers: { "Authorization": `Bearer ${accessToken}` },
    },
  );
  return resp.ok;
}

export const checkAccessCached = cachedConsult(checkAccess, (token) => token);
