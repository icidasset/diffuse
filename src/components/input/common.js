import { base64url } from "iso-base/rfc4648";

/**
 * @import {Track} from "@definitions/types.d.ts"
 */

/**
 * @param {{ fileUriOrScheme: string; handleFileUri: (args: { fileURI: string; tracks: Track[] }) => Track[]; inputScheme: string; tracks: Track[] }} _
 */
export function detach(
  { fileUriOrScheme, handleFileUri, inputScheme, tracks },
) {
  if (!fileUriOrScheme.includes("://")) {
    // Delete everything if scheme matches
    if (fileUriOrScheme === inputScheme) return [];
    return tracks;
  }

  return handleFileUri({ fileURI: fileUriOrScheme, tracks });
}

/**
 * @param {string} scheme
 * @param {string} groupId
 */
export async function groupKeyHash(scheme, groupId) {
  const rawBytes = new TextEncoder().encode(`${scheme}://${groupId}`);
  const hashedBytes = await crypto.subtle.digest("SHA-256", rawBytes);
  return base64url.encode(new Uint8Array(hashedBytes));
}

/**
 * @param {string} filename
 */
export function isAudioFile(filename) {
  return filename.match(/\.(flac|m4a|mp3|mp4|ogg|opus|wav|webm)$/);
}
