import foundation from "~/common/facets/foundation.js";
import * as CID from "~/common/cid.js";
import { createLoader, renderError } from "~/common/loader.js";

createLoader({
  $type: "sh.diffuse.output.theme",
  label: "Theme",
  source: () => {
    const output = foundation.orchestrator.output();
    return output.themes;
  },
  async render(theme) {
    if (theme.cid) {
      const valid = await CID.verify(
        new TextEncoder().encode(theme.html ?? ""),
        theme.cid,
      );

      if (!valid) {
        renderError(
          document.body,
          "CID mismatch: HTML content does not match the CID",
        );
        return;
      }
    }

    const iframe = document.createElement("iframe");
    iframe.srcdoc = theme.html ?? "";

    document.body.innerHTML = "";
    document.body.append(iframe);
  },
});
