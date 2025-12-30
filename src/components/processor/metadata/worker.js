import { ostiary, rpc } from "@common/worker.js";
import { musicMetadataTags } from "./common.js";

/**
 * @import { Actions, Extraction } from "./types.d.ts";
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {Actions['supply']}
 */
export async function supply(args) {
  // Construct records
  // TODO: Use other metadata lib as fallback: https://github.com/buzz/mediainfo.js
  return await musicMetadataTags(args).catch(
    /**
     * @param {Error} err
     * @returns {Extraction}
     */
    (err) => {
      console.warn("Metadata processor error:", err, args);
      return {};
    },
  );
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, {
    supply,
  });
});
