import { queryOptional } from "~/common/element.js";
import * as Output from "~/common/output.js";

/**
 * @import {DiffuseElement} from "~/common/element.js"
 * @import {OutputElement} from "~/components/output/types.d.ts"
 */

/**
 * Read a stored session value from output settings when an output is configured,
 * otherwise falls back to localStorage.
 *
 * @param {DiffuseElement} element
 * @param {string} key
 * @returns {Promise<string | null>}
 */
export async function readSession(element, key) {
  /** @type {OutputElement | null} */
  const output = queryOptional(element, "output-selector");

  if (output) {
    const settings = await Output.data(output.settings);
    const existing = settings.find((s) => s.key === key);
    if (existing) return existing.value;
  }

  return localStorage.getItem(key);
}

/**
 * Save a session value to output settings when an output is configured,
 * otherwise falls back to localStorage.
 *
 * @param {DiffuseElement} element
 * @param {string} key
 * @param {string} value
 */
export async function saveSession(element, key, value) {
  /** @type {OutputElement | null} */
  const output = queryOptional(element, "output-selector");

  if (output) {
    const settings = await Output.data(output.settings);
    const existing = settings.find((s) => s.key === key);

    const updated = existing
      ? settings.map((s) => s.key === key ? { ...s, value } : s)
      : [
        ...settings,
        {
          $type: /** @type {"sh.diffuse.output.setting"} */ (
            "sh.diffuse.output.setting"
          ),
          id: crypto.randomUUID(),
          key,
          value,
        },
      ];

    await output.settings.save(updated);
  } else {
    localStorage.setItem(key, value);
  }
}

/**
 * Remove a stored session value from output settings when an output is configured,
 * otherwise falls back to localStorage.
 *
 * @param {DiffuseElement} element
 * @param {string} key
 */
export async function clearSession(element, key) {
  /** @type {OutputElement | null} */
  const output = queryOptional(element, "output-selector");

  if (output) {
    const settings = await Output.data(output.settings);
    const updated = settings.filter((s) => s.key !== key);
    if (updated.length !== settings.length) {
      await output.settings.save(updated);
    }
  } else {
    localStorage.removeItem(key);
  }
}
