/**
 * Dropbox app key for the **output** app (app-folder scoped). This is a
 * separate Dropbox app from the input/upload app so the two OAuth flows
 * can be distinguished in the callback page.
 *
 * NOTE: The corresponding app secret is intentionally NOT included here.
 * The authorization-code-with-PKCE flow does not require it, and embedding
 * the secret in client-side code would expose it.
 */
export const APP_KEY = "te0c9pbeii8f8bw";

/**
 * localStorage key for the PKCE code verifier, stored during the
 * output app's authorization-code-with-PKCE OAuth flow and consumed by
 * the callback. Separate from the input's verifier key so the callback
 * can tell which flow is in progress.
 */
export const PKCE_VERIFIER_KEY = "oauth/callback/dropbox-output/code_verifier";

export const OBJECT_PREFIX = "";
