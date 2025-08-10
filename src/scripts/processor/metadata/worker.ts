import type { Extraction, Urls } from "./types.d.ts";
import { provide, transfer } from "@scripts/common";
import { musicMetadataTags } from "./common.ts";

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////
const actions = {
  supply,
};

const { tasks } = provide({
  actions,
  tasks: actions,
});

export type Actions = typeof actions;
export type Tasks = typeof tasks;

// Actions

async function supply(args: {
  includeArtwork?: boolean;
  mimeType?: string;
  stream?: ReadableStream;
  urls?: Urls;
}): Promise<Extraction> {
  // Construct records
  // TODO: Use other metadata lib as fallback: https://github.com/buzz/mediainfo.js
  const response = await musicMetadataTags(args).catch((err): Extraction => {
    console.warn("Metadata processor error:", err);
    console.log(args);

    return {};
  });

  // Fin
  return transfer(response);
}
