import { configureOAuth } from "@atcute/oauth-browser-client";

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

const STORAGE_KEY = "diffuse/output/raw/atproto/did";
const SCOPE = metadata.scope;
const STORAGE_NAME = "diffuse/output/raw/atproto/atcute/oauth";
const CLIENT_KEY = "diffuse/output/raw/atproto";

// CONFIGURE
// =========

const location = globalThis.location;

let redirect_uri = location.origin + location.pathname + location.search;

const isLocalDev = redirect_uri.startsWith("http://127.0.0.1");

if (!isLocalDev) {
  redirect_uri = location.origin + "/oauth/callback";
}

const client_id = isLocalDev
  ? `http://localhost/?redirect_uri=${encodeURIComponent(redirect_uri)}&scope=${
    encodeURIComponent(SCOPE)
  }`
  : /** @type {any} */ (import.meta).env?.ATPROTO_CLIENT_ID ??
    "https://elements.diffuse.sh/latest/components/output/raw/atproto/oauth-client-metadata.json";

configureOAuth({
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
});

// LOGIN
// =====

/**
 * Initiate the OAuth authorization flow for a given handle.
 * Navigates the browser away to the authorization server.
 *
 * @param {string} handle
 */
export async function login(handle) {
  const location = globalThis.location;

  sessionStorage.setItem(
    "oauth/callback/redirect_path",
    location.pathname + location.search,
  );
  sessionStorage.setItem("oauth/pending-client", CLIENT_KEY);

  const authUrl = await createAuthorizationUrl({
    target: { type: "account", identifier: /** @type {any} */ (handle) },
    scope: SCOPE,
  });

  location.assign(authUrl.toString());
}

// SESSION RESTORE / CALLBACK
// ==========================

/**
 * Attempt to restore an existing session or finalize an OAuth callback.
 * Returns the session if successful, or null if no session is available.
 *
 * @returns {Promise<Session | null>}
 */
export async function restoreOrFinalize() {
  const location = globalThis.location;

  // Check for OAuth callback parameters (the library uses response_mode=fragment,
  // so params arrive in the URL hash, not the query string)
  const params = new URLSearchParams(location.hash.slice(1));

  if (
    params.has("code") &&
    sessionStorage.getItem("oauth/pending-client") === CLIENT_KEY
  ) {
    sessionStorage.removeItem("oauth/pending-client");

    const result = await finalizeAuthorization(params);

    // Clean up URL (remove fragment containing OAuth params)
    history.replaceState(
      null,
      "",
      location.pathname + location.search,
    );

    // Persist the DID for future session restoration
    localStorage.setItem(STORAGE_KEY, result.session.info.sub);

    return result.session;
  }

  // Try to restore a previously stored session
  const did = localStorage.getItem(STORAGE_KEY);

  if (did) {
    try {
      return await getSession(
        /** @type {`did:${string}:${string}`} */ (did),
      );
    } catch (err) {
      console.warn(err);
      clearStoredSession();
      return null;
    }
  }

  return null;
}

// CLEAR SESSION
// =============

/**
 * Remove stored session data without contacting the server.
 * Used when the session has already been revoked.
 */
export function clearStoredSession() {
  const did = localStorage.getItem(STORAGE_KEY);

  if (did) {
    deleteStoredSession(/** @type {`did:${string}:${string}`} */ (did));
  }

  localStorage.removeItem(STORAGE_KEY);
}

// LOGOUT
// ======

/**
 * Sign out and revoke the current session.
 *
 * @param {OAuthUserAgent} agent
 */
export async function logout(agent) {
  const did = localStorage.getItem(STORAGE_KEY);

  try {
    await agent.signOut();
  } catch {
    if (did) {
      deleteStoredSession(
        /** @type {`did:${string}:${string}`} */ (did),
      );
    }
  }

  localStorage.removeItem(STORAGE_KEY);
}
