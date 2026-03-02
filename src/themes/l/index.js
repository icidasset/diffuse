import foundation from "~/common/facets/foundation.js";
import { createLoader } from "~/common/loader.js";

createLoader({
  $type: "sh.diffuse.output.theme",
  label: "Theme",
  source: () => {
    const output = foundation.orchestrator.output();
    return output.themes;
  },
  render(theme) {
    // TODO: Validate if CID matches HTML
    const iframe = document.createElement("iframe");
    iframe.srcdoc = theme.html ?? "";

    document.body.innerHTML = "";
    document.body.append(iframe);
  },
});
