import { configureOAuth } from "@atcute/oauth-browser-client";

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
} from "@atcute/oauth-browser-client";

export { OAuthUserAgent };

/**
 * @import {Session} from "@atcute/oauth-browser-client"
 */

const STORAGE_KEY = "dor-atproto:did";

// CONFIGURE
// =========

configureOAuth({
  metadata: {
    client_id: import.meta.env?.VITE_ATPROTO_CLIENT_ID ??
      "https://elements.diffuse.sh/oauth-client-metadata.json",
    redirect_uri:
      window.location.origin.replace("://localhost", "://127.0.0.1") +
      window.location.pathname,
  },
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
  const authUrl = await createAuthorizationUrl({
    target: { type: "account", identifier: /** @type {any} */ (handle) },
    scope: "atproto transition:generic",
  });

  window.location.assign(authUrl.toString());
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
  // Check for OAuth callback parameters (the library uses response_mode=fragment,
  // so params arrive in the URL hash, not the query string)
  const params = new URLSearchParams(window.location.hash.slice(1));

  if (params.has("code")) {
    const result = await finalizeAuthorization(params);

    // Clean up URL (remove fragment containing OAuth params)
    history.replaceState(
      null,
      "",
      window.location.pathname + window.location.search,
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
        { allowStale: true },
      );
    } catch {
      localStorage.removeItem(STORAGE_KEY);
      return null;
    }
  }

  return null;
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
