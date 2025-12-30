import { base64url } from "iso-base/rfc4648";

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
