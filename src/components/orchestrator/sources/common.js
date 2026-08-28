import { DISABLED_KEY } from "./constants.js";

/**
 * @import {Setting} from "~/definitions/types.d.ts"
 */

/**
 * Strips query params from a URI, returning the key used to store/compare disabled state.
 *
 * @param {string} uri
 * @returns {string}
 */
export function uriKey(uri) {
  const q = uri.indexOf("?");
  return q === -1 ? uri : uri.slice(0, q);
}

/**
 * Parses the disabled source URIs list from a settings array.
 * Returns an empty array if the setting is absent or malformed.
 *
 * @param {Setting[]} settings
 * @returns {string[]}
 */
export function parseDisabledUris(settings) {
  const setting = settings.find((s) => s.key === DISABLED_KEY);
  if (!setting) return [];
  try {
    const parsed = JSON.parse(setting.value);
    return Array.isArray(parsed) ? parsed : [];
  } catch {
    return [];
  }
}
