import { BrowserOAuthClient } from "@atproto/oauth-client-browser";

import metadata from "./oauth-client-metadata.json" with {
  type: "json",
};

/**
 * @import {OAuthSession} from "@atproto/oauth-client-browser"
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
  // Store the return path so the shared `/oauth/callback` page can send the
  // user back here (with the OAuth response fragment) once the authorization
  // server redirects. Without this the callback falls back to `/`, which
  // resolves to the latest version instead of the facet the user launched from.
  localStorage.setItem(
    "oauth/callback/redirect_path",
    location.pathname + location.search,
  );

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
  const c = await client();

  // In production the PDS redirects back to the registered
  // `/oauth/callback` URI, and the shared callback page then forwards us here
  // (the facet loader) with the OAuth response preserved in the URL hash. That
  // forwarding changes `location.pathname`, so `init()`'s internal
  // `findRedirectUrl()` no longer matches the registered redirect URI and it
  // silently skips the callback. Detect the response ourselves and finalize it
  // directly; `initCallback` falls back to `clientMetadata.redirect_uris[0]`
  // for the token exchange, which is the same URI `authorize` used.
  const params = c.readCallbackParams();
  if (params) {
    const result = await c.initCallback(params);
    return result.session ?? null;
  }

  // No OAuth response in the URL: restore any previously stored session.
  const result = await c.init();
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
