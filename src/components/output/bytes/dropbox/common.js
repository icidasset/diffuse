import { getAccessToken } from "~/components/input/dropbox/common.js";
import { APP_KEY } from "./constants.js";

/**
 * Dropbox API paths must be absolute (start with `/`) or empty for the app
 * folder root. The output element builds keys like `"facets"` or
 * `"namespace/facets"` (no leading slash, which is fine for S3 keys but
 * rejected by Dropbox), so normalize them here at the API boundary.
 *
 * @param {string} filePath
 */
function normalizePath(filePath) {
  if (!filePath) return "";
  return filePath.startsWith("/") ? filePath : "/" + filePath;
}

/**
 * Download a file from the Dropbox app folder as raw bytes.
 *
 * The access token is obtained (and cached/auto-renewed) via the shared
 * `getAccessToken` helper, passing the **output** app key so the correct
 * refresh token is used.
 *
 * @param {string} refreshToken - Long-lived refresh token for the output app.
 * @param {string} filePath - Path relative to the app folder root (e.g. "/tracks").
 * @returns {Promise<Uint8Array | undefined>} The file bytes, or undefined if the file doesn't exist.
 */
export async function getFile(refreshToken, filePath) {
  const path = normalizePath(filePath);
  const accessToken = await getAccessToken(refreshToken, APP_KEY);
  if (!accessToken) return undefined;

  const resp = await fetch(
    "https://content.dropboxapi.com/2/files/download",
    {
      method: "POST",
      headers: {
        "Authorization": `Bearer ${accessToken}`,
        "Dropbox-API-Arg": JSON.stringify({ path }),
      },
    },
  );

  // 409 from Dropbox means the file doesn't exist yet — not an error for us.
  if (!resp.ok) return undefined;

  const buffer = await resp.arrayBuffer();
  return new Uint8Array(buffer);
}

/**
 * Upload (overwrite) a file in the Dropbox app folder with raw bytes.
 *
 * Uses `mode: "overwrite"` because the output component syncs data files
 * (tracks, playlists, settings, facets) that should replace any previous
 * version, not be auto-renamed.
 *
 * @param {string} refreshToken - Long-lived refresh token for the output app.
 * @param {string} filePath - Path relative to the app folder root (e.g. "/tracks").
 * @param {Uint8Array} data - The bytes to upload.
 */
export async function putFile(refreshToken, filePath, data) {
  const path = normalizePath(filePath);
  const accessToken = await getAccessToken(refreshToken, APP_KEY);
  if (!accessToken) {
    throw new Error(
      "Dropbox access token could not be refreshed. Please reconnect.",
    );
  }

  const resp = await fetch(
    "https://content.dropboxapi.com/2/files/upload",
    {
      method: "POST",
      headers: {
        "Authorization": `Bearer ${accessToken}`,
        "Content-Type": "application/octet-stream",
        "Dropbox-API-Arg": JSON.stringify({
          path,
          mode: "overwrite",
          mute: true,
        }),
      },
      body: new Blob([/** @type {BlobPart} */ (data)]),
    },
  );

  if (!resp.ok) {
    const body = await resp.text().catch(() => "");
    throw new Error(
      `Failed to upload "${filePath}" to Dropbox (${resp.status}): ${body}`,
    );
  }
}
