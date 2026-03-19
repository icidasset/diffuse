import { registerIconLibrary } from "@awesome.me/webawesome/dist/components/icon/library.js";
import data from "@phosphor-icons/web/thin/selection.json" with {
  type: "json",
};

import { buildIconMap, makeSvgDataUrl } from "./_lib.js";

const map = buildIconMap(data);

registerIconLibrary("phosphor/thin", {
  resolver: (name) => {
    const icon = map.get(name);
    return icon ? makeSvgDataUrl(icon.paths, icon.attrs) : "";
  },
});
