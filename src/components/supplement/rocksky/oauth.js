import {
  configureOAuth,
  createAuthorizationUrl,
  deleteStoredSession,
  finalizeAuthorization,
  getSession,
  OAuthUserAgent,
  TokenRefreshError,
} from "@atcute/oauth-browser-client";
import { withOAuthContext } from "~/oauth/lock.js";

import {
  CompositeDidDocumentResolver,
  LocalActorResolver,
  PlcDidDocumentResolver,
  WebDidDocumentResolver,
  XrpcHandleResolver,
} from "@atcute/identity-resolver";

import metadata from "./oauth-client-metadata.json" with {
  type: "json",
};

/**
 * @import {Session} from "@atcute/oauth-browser-client"
 */

export { OAuthUserAgent, TokenRefreshError };

export const DID_STORAGE_KEY = "diffuse/supplement/rocksky/atproto/did";
const CLIENT_KEY = "diffuse/supplement/rocksky";

const SCOPE = metadata.scope;
const OWNER = "diffuse/supplement/rocksky";

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
  : /** @type {any} */ (import.meta).env?.ROCKSKY_ATPROTO_CLIENT_ID ??
    "https://diffuse.sh/latest/components/supplement/rocksky/oauth-client-metadata.json";

const OAUTH_CONFIG = {
  metadata: { client_id, redirect_uri },
  storageName: "diffuse/supplement/rocksky/atcute/oauth",
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
 * Configure the atcute library for Rocksky.
 *
 * `@atcute/oauth-browser-client` keeps its database and client id in module
 * globals, shared with every other module that imports it (notably the
 * `atproto-passkey` output). Whichever module called `configureOAuth()` last
 * owns the globals, and a token refresh reads and writes through them. Every
 * operation below therefore runs under `withOAuthContext()`, which holds the
 * shared lock and re-configures when this module doesn't already own them.
 */
function configure() {
  configureOAuth(OAUTH_CONFIG);
}

/**
 * Delete the stored session and the persisted DID.
 *
 * Must only be called while holding the OAuth lock, after `configure()`.
 */
function clearStoredSessionUnlocked() {
  const did = localStorage.getItem(DID_STORAGE_KEY);

  if (did) {
    deleteStoredSession(/** @type {`did:${string}:${string}`} */ (did));
  }

  localStorage.removeItem(DID_STORAGE_KEY);
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
 * Initiate the Rocksky OAuth authorization flow for a given handle.
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
 * Attempt to restore an existing Rocksky session or finalize an OAuth callback.
 *
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
    const params = new URLSearchParams(loc.hash.slice(1));

    if (
      params.has("code") &&
      localStorage.getItem("oauth/pending-client") === CLIENT_KEY
    ) {
      // Finalize first; only clear the pending marker once it succeeds. If the
      // page reloads mid-exchange the marker survives so the reloaded page can
      // retry instead of silently dropping the login.
      const result = await finalizeAuthorization(params);

      localStorage.removeItem("oauth/pending-client");

      history.replaceState(null, "", loc.pathname + loc.search);
      localStorage.setItem(DID_STORAGE_KEY, result.session.info.sub);

      return result.session;
    }

    const did = localStorage.getItem(DID_STORAGE_KEY);

    if (did) {
      try {
        return await getSession(asDid(did));
      } catch (err) {
        // A `TokenRefreshError` means the refresh token was rejected (revoked
        // or expired): the session is gone and the user has to sign in again.
        // Anything else is transient — keep the session, otherwise a flaky
        // network at page load would permanently log the user out and every
        // subsequent scrobble would fail silently.
        if (err instanceof TokenRefreshError) {
          console.warn("Rocksky: stored session is no longer valid", err);
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
 * Remove the stored session.
 *
 * @returns {Promise<void>}
 */
export function clearStoredSession() {
  return withOAuthContext(OWNER, configure, () => {
    clearStoredSessionUnlocked();
  });
}

// LOGOUT
// ======

/**
 * Revoke the stored session (best-effort) and remove it.
 *
 * @returns {Promise<void>}
 */
export function logout() {
  return withOAuthContext(OWNER, configure, async () => {

    const did = localStorage.getItem(DID_STORAGE_KEY);

    try {
      if (did) {
        const session = await getSession(asDid(did), { allowStale: true });
        await new OAuthUserAgent(session).signOut();
      }
    } catch {
      // Revocation failed (e.g. already revoked or offline); make sure the
      // local session is still removed below.
    }

    clearStoredSessionUnlocked();
  });
}

// AUTHORIZED REQUESTS
// ===================

/**
 * Run `fn` with a valid Rocksky session. The whole operation holds the OAuth
 * lock so the library keeps using the Rocksky database and client id while the
 * session is refreshed, and so another module can't swap them mid-request.
 *
 * @template T
 * @param {(session: Session) => Promise<T>} fn
 * @returns {Promise<T>}
 */
export function withSession(fn) {
  return withOAuthContext(OWNER, configure, async () => {

    const did = localStorage.getItem(DID_STORAGE_KEY);

    if (!did) {
      throw new Error("Rocksky: no stored session");
    }

    const session = await getSession(asDid(did));

    return await fn(session);
  });
}
