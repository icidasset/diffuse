import { BrowserOAuthClient } from "@atproto/oauth-client-browser";

import metadata from "./oauth-client-metadata.json" with {
  type: "json",
};

/**
 * @import {OAuthSession} from "@atproto/oauth-client"
 */

const SCOPE = metadata.scope;

// Local development uses a loopback client id so the redirect lands back on the
// current origin (including the dev-server port). The loopback client id must
// carry the redirect target in its query string and have no path; the scope is
// passed later at sign-in. Otherwise prefer the per-component client id (e.g.
// the cimd-service development client) provided via the environment, falling
// back to the published client metadata.
const isLocalDev = location.origin.startsWith("http://127.0.0.1");

const redirect_uri = location.origin + location.pathname + location.search;

const CLIENT_ID = isLocalDev
  ? `http://localhost/?redirect_uri=${encodeURIComponent(redirect_uri)}&scope=${
    encodeURIComponent(SCOPE)
  }`
  : /** @type {any} */ (import.meta).env?.ATPROTO_SPACE_CLIENT_ID ??
    metadata.client_id;

/** @type {Promise<BrowserOAuthClient> | null} */
let clientPromise = null;

/**
 * Lazily create the browser OAuth client, configured with the space-aware
 * client metadata. The client persists its session (DPoP keys, token set) in
 * IndexedDB keyed by `client_id`.
 */
function client() {
  // `load` derives the client metadata (including the default scope) from the
  // published `client_id`, so no metadata is supplied here. A handle resolver
  // is required so the client can resolve handles to DIDs; it mirrors the URL
  // used by the other AT Protocol outputs.
  clientPromise ??= BrowserOAuthClient.load({
    clientId: CLIENT_ID,
    handleResolver: "https://public.api.bsky.app",
  });

  return clientPromise;
}

// LOGIN
// =====

/**
 * Initiate the OAuth authorization flow for a given handle.
 *
 * @param {string} handle
 * @returns {Promise<OAuthSession>}
 */
export function login(handle) {
  return client().then((c) => c.signIn(handle, { scope: SCOPE }));
}

// SESSION RESTORE / CALLBACK
// ==========================

/**
 * Attempt to restore an existing session or finalize an OAuth callback.
 *
 * @returns {Promise<OAuthSession | null>}
 */
export async function restoreOrFinalize() {
  const result = await (await client()).init();
  return result?.session ?? null;
}

// LOGOUT
// ======

/**
 * Sign out and revoke the current session.
 *
 * @param {OAuthSession} session
 */
export async function logout(session) {
  await session.signOut();
}
