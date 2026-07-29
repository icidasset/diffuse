import * as URI from "fast-uri";
import QS from "query-string";

import { cachedConsult, isAudioFile } from "~/components/input/common.js";
import { safeDecodeURIComponent } from "~/common/utils.js";
import {
  ACCESS_TOKEN_REFRESH_MARGIN_MS,
  DEFAULT_APP_KEY,
  SCHEME,
} from "./constants.js";

/**
 * @import { Track } from "~/definitions/types.d.ts"
 * @import { ConsultResult } from "@specs/components/input/types.d.ts"
 */

/**
 * @typedef {{ refreshToken: string; directoryPath: string }} Account
 */

////////////////////////////////////////////
// PKCE
////////////////////////////////////////////

/**
 * Base64url-encode raw bytes (no padding), as required by PKCE.
 *
 * @param {Uint8Array} bytes
 */
function base64url(bytes) {
  return btoa(String.fromCharCode(...bytes))
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/=+$/, "");
}

/**
 * Generates a PKCE code verifier and its S256 challenge.
 *
 * The verifier is a random string the client keeps secret; the challenge
 * is `base64url(SHA-256(verifier))` and is sent to the authorization
 * server during the authorize request. When exchanging the code, the
 * verifier proves the client is the same one that started the flow.
 *
 * @returns {Promise<{ verifier: string; challenge: string }>}
 */
export async function generatePKCEPair() {
  const bytes = crypto.getRandomValues(new Uint8Array(32));
  const verifier = base64url(bytes);

  const digest = await crypto.subtle.digest("SHA-256", new TextEncoder().encode(verifier));
  const challenge = base64url(new Uint8Array(digest));

  return { verifier, challenge };
}

////////////////////////////////////////////
// TOKEN EXCHANGE & REFRESH
////////////////////////////////////////////

/**
 * Exchange an authorization code for a refresh token (and short-lived
 * access token) using the PKCE code verifier.
 *
 * @param {string} code - The authorization code returned by Dropbox.
 * @param {string} codeVerifier - The PKCE code verifier stored during authorize().
 * @param {string} [redirectUri] - The redirect URI registered with Dropbox.
 * @param {string} [appKey] - The Dropbox app key.
 * @returns {Promise<{ refreshToken: string; accessToken: string } | null>}
 */
export async function exchangeCode(code, codeVerifier, redirectUri, appKey = DEFAULT_APP_KEY) {
  const params = new URLSearchParams({
    code,
    grant_type: "authorization_code",
    client_id: appKey,
    redirect_uri: redirectUri ?? (location.origin + "/oauth/callback/"),
    code_verifier: codeVerifier,
  });

  const resp = await fetch("https://api.dropboxapi.com/oauth2/token", {
    method: "POST",
    headers: { "Content-Type": "application/x-www-form-urlencoded" },
    body: params,
  });

  if (!resp.ok) return null;

  /** @type {{ access_token: string; refresh_token: string }} */
  const data = await resp.json();
  return { refreshToken: data.refresh_token, accessToken: data.access_token };
}

/**
 * In-memory cache of access tokens keyed by refresh token.
 * Each entry stores the token and its expiry timestamp.
 *
 * @type {Map<string, { accessToken: string; expiresAt: number; inflight: Promise<string | null> | null }>}
 */
const accessTokenCache = new Map();

/**
 * Exchanges a refresh token for a fresh short-lived access token.
 *
 * Results are cached and reused until close to expiry. Concurrent calls
 * for the same refresh token share a single network request.
 *
 * @param {string} refreshToken
 * @param {string} [appKey]
 * @returns {Promise<string | null>} The access token, or null if the refresh failed.
 */
export function getAccessToken(refreshToken, appKey = DEFAULT_APP_KEY) {
  const now = Date.now();
  const cached = accessTokenCache.get(refreshToken);

  // Return cached token if still valid (with a safety margin).
  if (cached && cached.expiresAt > now + ACCESS_TOKEN_REFRESH_MARGIN_MS) {
    return Promise.resolve(cached.accessToken);
  }

  // If a refresh is already in flight for this token, piggyback on it.
  if (cached?.inflight) {
    return cached.inflight;
  }

  const inflight = (async () => {
    try {
      const params = new URLSearchParams({
        grant_type: "refresh_token",
        refresh_token: refreshToken,
        client_id: appKey,
      });

      const resp = await fetch("https://api.dropboxapi.com/oauth2/token", {
        method: "POST",
        headers: { "Content-Type": "application/x-www-form-urlencoded" },
        body: params,
      });

      if (!resp.ok) return null;

      /** @type {{ access_token: string; expires_in: number }} */
      const data = await resp.json();
      const accessToken = data.access_token;
      const expiresAt = now + data.expires_in * 1000;

      accessTokenCache.set(refreshToken, { accessToken, expiresAt, inflight: null });
      return accessToken;
    } catch {
      return null;
    }
  })();

  if (cached) {
    cached.inflight = inflight;
  } else {
    accessTokenCache.set(refreshToken, { accessToken: "", expiresAt: 0, inflight });
  }

  return inflight;
}

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
    userinfo: encodeURIComponent(account.refreshToken),
    host: "dropbox.com",
    path: filePath || "/",
    query: QS.stringify({ dir: account.directoryPath || "/" }),
  });
}

/**
 * @param {string} uriString
 * @returns {{ refreshToken: string; path: string; directoryPath: string } | undefined}
 */
export function parseURI(uriString) {
  const uri = URI.parse(uriString);
  if (uri.scheme !== SCHEME) return undefined;
  if (!uri.userinfo) return undefined;

  const refreshToken = decodeURIComponent(uri.userinfo);
  const path = safeDecodeURIComponent(uri.path || "/");
  const qs = QS.parse(uri.query || "");
  const directoryPath = typeof qs.dir === "string" ? safeDecodeURIComponent(qs.dir) : "/";

  return { refreshToken, path, directoryPath };
}

////////////////////////////////////////////
// ACCOUNT HELPERS
////////////////////////////////////////////

/**
 * @param {Account} account
 */
export function accountId(account) {
  return `${account.refreshToken}:${account.directoryPath}`;
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

    acc[id] = { refreshToken: parsed.refreshToken, directoryPath: parsed.directoryPath };
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
        account: { refreshToken: parsed.refreshToken, directoryPath: parsed.directoryPath },
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
        account: { refreshToken: parsed.refreshToken, directoryPath: parsed.directoryPath },
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
 * @param {string} refreshToken
 * @param {string} directoryPath
 * @returns {Promise<Array<{ name: string; path_lower: string }> | null>}
 */
export async function listFiles(refreshToken, directoryPath) {
  const accessToken = await getAccessToken(refreshToken);
  if (!accessToken) return null;

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
 * @param {string} refreshToken
 * @param {string} filePath
 * @returns {Promise<string | null>}
 */
export async function getTemporaryLink(refreshToken, filePath) {
  const accessToken = await getAccessToken(refreshToken);
  if (!accessToken) return null;

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
 * @param {string} refreshToken
 * @returns {Promise<ConsultResult>}
 */
export async function checkAccess(refreshToken) {
  const accessToken = await getAccessToken(refreshToken);
  if (!accessToken) return "no";

  try {
    const resp = await fetch(
      "https://api.dropboxapi.com/2/users/get_current_account",
      {
        method: "POST",
        headers: { "Authorization": `Bearer ${accessToken}` },
      },
    );
    return resp.ok ? "yes" : "no";
  } catch {
    // Network error: inconclusive — let `cachedConsult` keep the last
    // known availability rather than caching a sticky "no".
    return "unsure";
  }
}

export const checkAccessCached = cachedConsult(checkAccess, (token) => token);
