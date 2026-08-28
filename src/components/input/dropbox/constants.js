export const SCHEME = "dropbox";
export const DEFAULT_APP_KEY = "kwsydtrzban41zr";

/**
 * Dropbox short-lived access tokens last ~4 hours. We refresh a bit
 * early (5 minutes before expiry) to avoid edge-case failures where
 * the token expires between the refresh check and the API call.
 */
export const ACCESS_TOKEN_REFRESH_MARGIN_MS = 5 * 60 * 1000;

/**
 * localStorage key for the PKCE code verifier, stored during the
 * authorization-code-with-PKCE OAuth flow and consumed by the callback.
 */
export const PKCE_VERIFIER_KEY = "oauth/callback/dropbox/code_verifier";
