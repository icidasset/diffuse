import {
  configureOAuth,
  createAuthorizationUrl,
  deleteStoredSession,
  finalizeAuthorization,
  getSession,
  OAuthUserAgent,
} from "@atcute/oauth-browser-client";

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

export { getSession, OAuthUserAgent };

export const DID_STORAGE_KEY = "diffuse/supplement/rocksky/atproto/did";
const CLIENT_KEY = "diffuse/supplement/rocksky";

const SCOPE = metadata.scope;

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
    "https://elements.diffuse.sh/latest/components/supplement/rocksky/oauth-client-metadata.json";

configureOAuth({
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
});

// LOGIN
// =====

/**
 * Initiate the Rocksky OAuth authorization flow for a given handle.
 * Navigates the browser away to the authorization server.
 *
 * @param {string} handle
 */
export async function login(handle) {
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
 * Attempt to restore an existing Rocksky session or finalize an OAuth callback.
 * Returns the session if successful, or null if no session is available.
 *
 * @returns {Promise<Session | null>}
 */
export async function restoreOrFinalize() {
  const params = new URLSearchParams(location.hash.slice(1));

  if (
    params.has("code") &&
    sessionStorage.getItem("oauth/pending-client") === CLIENT_KEY
  ) {
    sessionStorage.removeItem("oauth/pending-client");

    const result = await finalizeAuthorization(params);

    history.replaceState(null, "", location.pathname + location.search);
    localStorage.setItem(DID_STORAGE_KEY, result.session.info.sub);

    return result.session;
  }

  const did = localStorage.getItem(DID_STORAGE_KEY);

  if (did) {
    try {
      return await getSession(/** @type {`did:${string}:${string}`} */ (did));
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

export function clearStoredSession() {
  const did = localStorage.getItem(DID_STORAGE_KEY);

  if (did) {
    deleteStoredSession(/** @type {`did:${string}:${string}`} */ (did));
  }

  localStorage.removeItem(DID_STORAGE_KEY);
}

// LOGOUT
// ======

/**
 * @param {OAuthUserAgent} agent
 */
export async function logout(agent) {
  const did = localStorage.getItem(DID_STORAGE_KEY);

  try {
    await agent.signOut();
  } catch {
    if (did) {
      deleteStoredSession(/** @type {`did:${string}:${string}`} */ (did));
    }
  }

  localStorage.removeItem(DID_STORAGE_KEY);
}
