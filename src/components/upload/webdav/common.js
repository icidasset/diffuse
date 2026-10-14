import {
  authHeader,
  parseURI,
  toHttpUrl,
} from "~/components/input/webdav/common.js";

/**
 * @import { Server } from "@specs/components/input/webdav/types.d.ts"
 */

////////////////////////////////////////////
// WEBDAV API
////////////////////////////////////////////

/**
 * Upload a file to a WebDAV server. Overwrites any existing resource at the
 * same path — safe because uploads are content-addressed (the filename is
 * `<CID>.<ext>`), so the same bytes always map to the same path (overwrite is
 * a no-op) and different bytes get a different path. This makes re-uploads
 * idempotent.
 *
 * @param {Server} server
 * @param {string} destinationPath Full server path (e.g. "/Music/song.mp3").
 * @param {File} file
 * @returns {Promise<{ path: string }>} The path of the uploaded file.
 */
export async function uploadFile(server, destinationPath, file) {
  try {
    const response = await fetch(toHttpUrl(server, destinationPath), {
      method: "PUT",
      headers: {
        "Authorization": authHeader(server),
        ...(file.type ? { "Content-Type": file.type } : {}),
      },
      body: file,
    });

    if (!response.ok) {
      throw new Error(
        `Unexpected response code from the server (${response.status} ${response.statusText}).`,
      );
    }
  } catch (err) {
    const detail = err instanceof Error && err.message
      ? ` (${err.message})`
      : "";
    throw new Error(
      `Failed to upload "${file.name}" to WebDAV server "${server.host}"${detail}`,
    );
  }

  return { path: destinationPath };
}

/**
 * Delete a file from a WebDAV server.
 *
 * @param {Server} server
 * @param {string} filePath Full server path (e.g. "/Music/song.mp3").
 * @returns {Promise<boolean>}
 */
export async function deleteFile(server, filePath) {
  try {
    const response = await fetch(toHttpUrl(server, filePath), {
      method: "DELETE",
      headers: {
        "Authorization": authHeader(server),
      },
    });

    if (!response.ok) {
      throw new Error(
        `Unexpected response code from the server (${response.status} ${response.statusText}).`,
      );
    }
  } catch (err) {
    const detail = err instanceof Error && err.message
      ? ` (${err.message})`
      : "";
    throw new Error(
      `Failed to delete "${filePath}" from WebDAV server "${server.host}"${detail}`,
    );
  }

  return true;
}

////////////////////////////////////////////
// PATH HELPERS
////////////////////////////////////////////

/**
 * Resolve the destination path for an upload.
 *
 * If `path` is provided it is used as-is (a full server path). Otherwise the
 * file is placed inside the server's configured directory — the directory the
 * matching input component lists files under.
 *
 * @param {Server} server
 * @param {File} file
 * @param {string} [path]
 * @returns {string}
 */
export function resolveDestinationPath(server, file, path) {
  if (path) return path;

  const dir = server.dir.replace(/\/+$/, "");
  return `${dir}/${file.name}`;
}

/**
 * Extract the server config from an account URI.
 * Throws if the URI is not a valid WebDAV URI.
 *
 * @param {string} uri
 * @returns {Server}
 */
export function serverFromURI(uri) {
  const parsed = parseURI(uri);
  if (!parsed) {
    throw new Error(`Invalid WebDAV URI: ${uri}`);
  }
  return parsed.server;
}

/**
 * Extract the server config and file path from a track URI.
 * Throws if the URI is not a valid WebDAV file URI.
 *
 * @param {string} uri
 * @returns {{ server: Server; path: string }}
 */
export function serverAndPathFromURI(uri) {
  const parsed = parseURI(uri);
  if (!parsed) {
    throw new Error(`Invalid WebDAV URI: ${uri}`);
  }
  if (!parsed.path) {
    throw new Error(`Invalid WebDAV file URI: ${uri}`);
  }

  return { server: parsed.server, path: parsed.path };
}
