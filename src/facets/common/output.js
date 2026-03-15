import { effect } from "~/common/signal.js";
import { insertPreludes } from "~/common/facets/prelude.js";
import foundation from "~/common/facets/foundation.js";

const elements = document.createElement("div");
elements.id = "elements-container";
document.body.appendChild(elements);

let preludeInserted = false;
let preludeLoaded = false;

effect(() => {
  const out = foundation.signals.orchestrator.output();
  if (!out) return;

  const col = out.facets.collection();
  if (col.state !== "loaded") return;

  if (preludeInserted === false) {
    preludeInserted = true;
    insertPreludes(col.data, elements).then(() => {
      preludeLoaded = true;
    });
  }
});

export async function output() {
  const out = await foundation.orchestrator.output();

  if (out.hasSelected() && !out.selected() && !preludeLoaded) {
    await new Promise((resolve) => {
      let resolved = false;

      const stop = effect(() => {
        if (resolved) {
          stop();
          return;
        }

        const s = out.selected();

        if (s) {
          resolved = true;
          resolve(null);
        }
      });
    });
  }

  return out;
}
