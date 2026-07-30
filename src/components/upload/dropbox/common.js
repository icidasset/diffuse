import { parseURI, getAccessToken } from "~/components/input/dropbox/common.js";

/**
 * @import { Account } from "~/components/input/dropbox/common.js"
 */

////////////////////////////////////////////
// DROPBOX API
////////////////////////////////////////////

/**
 * Upload a file to Dropbox. Uses the content-upload endpoint with the
 * file's bytes as the request body.
 *
 * @param {string} refreshToken
 * @param {string} destinationPath Full Dropbox path (e.g. "/Music/song.mp3").
 * @param {File} file
 * @returns {Promise<{ path_lower: string; name: string } | null>} The uploaded file metadata, or null on failure.
 */
export async function uploadFile(refreshToken, destinationPath, file) {
  const accessToken = await getAccessToken(refreshToken);
  if (!accessToken) {
    throw new Error("Dropbox access token could not be refreshed. Please reconnect.");
  }

  const resp = await fetch(
    "https://content.dropboxapi.com/2/files/upload",
    {
      method: "POST",
      headers: {
        "Authorization": `Bearer ${accessToken}`,
        "Content-Type": "application/octet-stream",
        // `overwrite` is safe because uploads are content-addressed: the
        // filename is `<CID>.<ext>`, so the same bytes always map to the same
        // path (overwrite is a no-op) and different bytes get a different path
        // (no conflict). This makes re-uploads idempotent instead of spawning
        // autorenamed duplicates.
        "Dropbox-API-Arg": JSON.stringify({
          path: destinationPath,
          mode: "overwrite",
          mute: true,
        }).replace(/[^\x00-\x7F]/g, (ch) =>
          "\\u" + ("0000" + ch.charCodeAt(0).toString(16)).slice(-4)
        ),
      },
      body: file,
    },
  );

  if (!resp.ok) {
    /** @type {{ error?: { ".tag"?: string } } | null} */
    const body = await resp.json().catch(() => null);
    if (body?.error?.[".tag"] === "expired_access_token") {
      throw new Error("Dropbox access token has expired. Please reconnect.");
    }
    return null;
  }

  /** @type {{ path_lower: string; name: string }} */
  const data = await resp.json();
  return data;
}

/**
 * Delete a file from Dropbox.
 *
 * @param {string} refreshToken
 * @param {string} filePath Full Dropbox path (e.g. "/Music/song.mp3").
 * @returns {Promise<boolean>}
 */
export async function deleteFile(refreshToken, filePath) {
  const accessToken = await getAccessToken(refreshToken);
  if (!accessToken) {
    throw new Error("Dropbox access token could not be refreshed. Please reconnect.");
  }

  const resp = await fetch(
    "https://api.dropboxapi.com/2/files/delete_v2",
    {
      method: "POST",
      headers: {
        "Authorization": `Bearer ${accessToken}`,
        "Content-Type": "application/json",
      },
      body: JSON.stringify({ path: filePath }),
    },
  );

  if (!resp.ok) {
    /** @type {{ error?: { ".tag"?: string } } | null} */
    const body = await resp.json().catch(() => null);
    if (body?.error?.[".tag"] === "expired_access_token") {
      throw new Error("Dropbox access token has expired. Please reconnect.");
    }
    throw new Error(`Failed to delete "${filePath}" from Dropbox`);
  }

  return true;
}

////////////////////////////////////////////
// PATH HELPERS
////////////////////////////////////////////

/**
 * Resolve the destination path for an upload.
 *
 * If `path` is provided it is used as-is (a full Dropbox path).
 * Otherwise the file is placed inside the account's directory.
 *
 * @param {Account} account
 * @param {File} file
 * @param {string} [path]
 * @returns {string}
 */
export function resolveDestinationPath(account, file, path) {
  if (path) return path;

  const dir = account.directoryPath === "/"
    ? ""
    : account.directoryPath;
  return `${dir}/${file.name}`;
}

/**
 * Extract the account info from an account URI.
 * Throws if the URI is not a valid Dropbox account URI.
 *
 * @param {string} uri
 * @returns {Account}
 */
export function accountFromURI(uri) {
  const parsed = parseURI(uri);
  if (!parsed) {
    throw new Error(`Invalid Dropbox URI: ${uri}`);
  }
  return { refreshToken: parsed.refreshToken, directoryPath: parsed.directoryPath };
}
