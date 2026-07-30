import { DEFAULT_APP_KEY, PKCE_VERIFIER_KEY } from "~/components/input/dropbox/constants.js";
import { exchangeCode } from "~/components/input/dropbox/common.js";
import { APP_KEY as OUTPUT_APP_KEY, PKCE_VERIFIER_KEY as OUTPUT_PKCE_VERIFIER_KEY } from "~/components/output/bytes/dropbox/constants.js";

const prefix = "oauth/callback";
const redirect_path = localStorage.getItem(`${prefix}/redirect_path`) ?? "/";

localStorage.removeItem(`${prefix}/redirect_path`);

// Dropbox uses the authorization-code flow with PKCE, so the `code` arrives
// as a query parameter (?code=...). Other providers (ATProto, Last.fm) use
// response_mode=fragment and arrive in the hash. We detect the Dropbox flow
// by the presence of a stored PKCE verifier and exchange the code here so
// the redirect target receives a `#refresh_token=...` (input) or
// `#output_refresh_token=...` (output) hash — keeping the consuming
// facets simple.
//
// Two separate Dropbox apps can be in play (input/upload vs output), each
// with its own verifier key. Only one flow can be active at a time (the user
// is redirected away mid-flow), so we check the output verifier first and
// fall back to the input verifier.
const code = new URLSearchParams(location.search).get("code");
const outputDropboxVerifier = localStorage.getItem(OUTPUT_PKCE_VERIFIER_KEY);
const dropboxVerifier = localStorage.getItem(PKCE_VERIFIER_KEY);

if (code && outputDropboxVerifier) {
  localStorage.removeItem(OUTPUT_PKCE_VERIFIER_KEY);

  const result = await exchangeCode(
    code,
    outputDropboxVerifier,
    location.origin + "/oauth/callback/",
    OUTPUT_APP_KEY,
  );

  if (result) {
    const hashParams = new URLSearchParams();
    hashParams.set("output_refresh_token", result.refreshToken);
    location.assign(`${redirect_path}#${hashParams.toString()}`);
  } else {
    location.assign(`${redirect_path}#error=dropbox_auth_failed`);
  }
} else if (code && dropboxVerifier) {
  localStorage.removeItem(PKCE_VERIFIER_KEY);

  const result = await exchangeCode(
    code,
    dropboxVerifier,
    location.origin + "/oauth/callback/",
    DEFAULT_APP_KEY,
  );

  if (result) {
    const hashParams = new URLSearchParams();
    hashParams.set("refresh_token", result.refreshToken);
    location.assign(`${redirect_path}#${hashParams.toString()}`);
  } else {
    location.assign(`${redirect_path}#error=dropbox_auth_failed`);
  }
} else {
  location.assign(`${redirect_path}${location.hash}`);
}
