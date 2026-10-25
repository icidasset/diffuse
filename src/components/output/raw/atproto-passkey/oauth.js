import { configureOAuth } from "@atcute/oauth-browser-client";
import { withOAuthContext } from "~/oauth/lock.js";

import metadata from "./oauth-client-metadata.json" with {
  type: "json",
};

import {
  CompositeDidDocumentResolver,
  LocalActorResolver,
  PlcDidDocumentResolver,
  WebDidDocumentResolver,
  XrpcHandleResolver,
} from "@atcute/identity-resolver";

import {
  createAuthorizationUrl,
  deleteStoredSession,
  finalizeAuthorization,
  getSession,
  OAuthUserAgent,
  TokenRefreshError,
} from "@atcute/oauth-browser-client";

export { OAuthUserAgent, TokenRefreshError };

/**
 * @import {Session} from "@atcute/oauth-browser-client"
 */

const STORAGE_KEY = "diffuse/output/raw/atproto-passkey/did";
const SCOPE = metadata.scope;
const STORAGE_NAME = "diffuse/output/raw/atproto-passkey/atcute/oauth";
const CLIENT_KEY = "diffuse/output/raw/atproto-passkey";
const OWNER = "diffuse/output/raw/atproto-passkey";

// CONFIGURE
// =========

let redirect_uri = location.origin + location.pathname + location.search;

const isLocalDev = redirect_uri.startsWith("http://127.0.0.1");

if (!isLocalDev) {
  redirect_uri = location.origin + "/oauth/callback";
}

const client_id = isLocalDev
  ? `http://localhost/?redirect_uri=${encodeURIComponent(redirect_uri)}&scope=${
    encodeURIComponent(SCOPE)
  }`
  : /** @type {any} */ (import.meta).env?.ATPROTO_PUBLIC_CLIENT_ID ??
    "https://diffuse.sh/latest/components/output/raw/atproto-passkey/oauth-client-metadata.json";

const OAUTH_CONFIG = {
  metadata: {
    client_id,
    redirect_uri,
  },
  storageName: STORAGE_NAME,
  identityResolver: new LocalActorResolver({
    handleResolver: new XrpcHandleResolver({
      serviceUrl: "https://public.api.bsky.app",
    }),
    didDocumentResolver: new CompositeDidDocumentResolver({
      methods: {
        plc: new PlcDidDocumentResolver(),
        web: new WebDidDocumentResolver(),
      },
    }),
  }),
};

/**
 * Configure the atcute library for the public AT Protocol output.
 *
 * `@atcute/oauth-browser-client` keeps its database and client id in module
 * globals shared with the Rocksky scrobbler. Whichever module called
 * `configureOAuth()` last owns the globals, and token refresh reads and writes
 * through them. Every operation must therefore re-configure while holding the
 * shared lock, otherwise a Rocksky operation can make this output refresh the
 * wrong session (or use the wrong client id) and stop syncing.
 */
function configure() {
  configureOAuth(OAUTH_CONFIG);
}

/**
 * Run `fn` with the library configured for this module, holding the shared
 * OAuth lock for the whole operation.
 *
 * @template T
 * @param {() => T | Promise<T>} fn
 * @returns {Promise<T>}
 */
export function withOAuth(fn) {
  return withOAuthContext(OWNER, configure, fn);
}

/**
 * Delete the stored session and the persisted DID.
 *
 * Must only be called while holding the OAuth lock, after `configure()`.
 */
function clearStoredSessionUnlocked() {
  const did = localStorage.getItem(STORAGE_KEY);

  if (did) {
    deleteStoredSession(asDid(did));
  }

  localStorage.removeItem(STORAGE_KEY);
}

/**
 * @param {string} did
 * @returns {`did:${string}:${string}`}
 */
function asDid(did) {
  return /** @type {`did:${string}:${string}`} */ (did);
}

// LOGIN
// =====

/**
 * Initiate the OAuth authorization flow for a given handle.
 * Navigates the browser away to the authorization server.
 *
 * @param {string} handle
 */
export function login(handle) {
  return withOAuthContext(OWNER, configure, async () => {

    localStorage.setItem(
      "oauth/callback/redirect_path",
      location.pathname + location.search,
    );

    localStorage.setItem("oauth/pending-client", CLIENT_KEY);

    const authUrl = await createAuthorizationUrl({
      target: { type: "account", identifier: /** @type {any} */ (handle) },
      scope: SCOPE,
    });

    location.assign(authUrl.toString());
  });
}

// SESSION RESTORE / CALLBACK
// ==========================

/**
 * Attempt to restore an existing session or finalize an OAuth callback.
 * Returns the session if successful, or `null` if there is no session to
 * restore. Transient failures (offline, timeouts, auth server errors) are
 * re-thrown and leave the stored session untouched, so a later attempt can
 * still refresh it. The stored session is only discarded when it is
 * definitively unusable (revoked / expired refresh token).
 *
 * @returns {Promise<Session | null>}
 */
export function restoreOrFinalize() {
  return withOAuthContext(OWNER, configure, async () => {

    const loc = globalThis.location;

    // Check for OAuth callback parameters (the library uses response_mode=fragment,
    // so params arrive in the URL hash, not the query string)
    const params = new URLSearchParams(loc.hash.slice(1));

    if (
      params.has("code") &&
      localStorage.getItem("oauth/pending-client") === CLIENT_KEY
    ) {
      // Finalize first; only clear the pending marker once it succeeds. If the
      // page reloads mid-exchange (e.g. a prelude change), the marker survives
      // so the reloaded page can retry instead of silently dropping the login.
      const result = await finalizeAuthorization(params);

      localStorage.removeItem("oauth/pending-client");

      // Clean up URL (remove fragment containing OAuth params)
      history.replaceState(null, "", loc.pathname + loc.search);

      // Persist the DID for future session restoration
      localStorage.setItem(STORAGE_KEY, result.session.info.sub);

      return result.session;
    }

    // Try to restore a previously stored session
    const did = localStorage.getItem(STORAGE_KEY);

    if (did) {
      try {
        return await getSession(asDid(did));
      } catch (err) {
        // A `TokenRefreshError` means the refresh token was rejected (revoked
        // or expired). Anything else is transient: keep the stored session so
        // a flaky network at page load doesn't permanently log the user out
        // and leave the output looking connected while it syncs nothing.
        if (err instanceof TokenRefreshError) {
          console.warn("ATProto: stored session is no longer valid", err);
          clearStoredSessionUnlocked();
          return null;
        }

        throw err;
      }
    }

    return null;
  });
}

// CLEAR SESSION
// =============

/**
 * Remove stored session data without contacting the server.
 * Used when the session has already been revoked.
 *
 * @returns {Promise<void>}
 */
export function clearStoredSession() {
  return withOAuth(() => {
    clearStoredSessionUnlocked();
  });
}

// LOGOUT
// ======

/**
 * Sign out and revoke the current session.
 *
 * @param {OAuthUserAgent} agent
 */
export function logout(agent) {
  return withOAuth(async () => {
    const did = localStorage.getItem(STORAGE_KEY);

    try {
      await agent.signOut();
    } catch {
      // Revocation failed (e.g. already revoked or offline); the local session
      // is still removed below.
      if (did) {
        deleteStoredSession(asDid(did));
      }
    }

    localStorage.removeItem(STORAGE_KEY);
  });
}
