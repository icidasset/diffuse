import * as Automerge from "@automerge/automerge";
import { base64 } from "iso-base/rfc4648";

/**
 * @import { TracksDocument } from "./types.d.ts";
 */

/** @type {Automerge.Doc<TracksDocument>} */
export const INITIAL_TRACKS_DOCUMENT = Automerge.load(
  base64.decode(
    "hW9Kg3QEcPYAeAEQhsIBj6DgCDtXSHEiZhcqigHxj0/xVpP8KdUJQ8e6qVEgaz7v6CpLuCGB58iHmx4plQYBAgMCEwIjBkACVgIHFQwhAiMCNAFCAlYCgAECfwB/AX8Bf9Xbz8sGfwB/B38KY29sbGVjdGlvbn8AfwEBfwJ/AH8AAA",
  ),
);
