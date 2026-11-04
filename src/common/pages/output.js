import { effect, signal } from "~/common/signal.js";
import { insertPreludes } from "~/common/facets/prelude.js";
import foundation from "~/common/foundation.js";

const elements = document.createElement("div");
elements.id = "elements-container";
document.body.appendChild(elements);

let preludeInserted = false;
const preludeLoaded = signal(false);

effect(() => {
  const out = foundation.signals.orchestrator.output();
  if (!out) return;

  const col = out.facets.collection();
  if (col.state !== "loaded") return;

  if (preludeInserted === false) {
    preludeInserted = true;
    insertPreludes(col.data, elements)
      .catch((err) => {
        console.error("Failed to insert preludes:", err);
      })
      .finally(() => {
        preludeLoaded.set(true);
      });
  }
});

export async function output() {
  const out = await foundation.orchestrator.output();

  if (out.hasSelected() && !out.selected() && !preludeLoaded.get()) {
    await new Promise((resolve) => {
      let resolved = false;

      const stop = effect(() => {
        if (resolved) {
          stop();
          return;
        }

        // Resolve once the selected output appears, or once the preludes have
        // been inserted. The latter keeps a missing/broken output element from
        // leaving the dashboard on its loading spinner forever.
        if (out.selected() || preludeLoaded.get()) {
          resolved = true;
          resolve(null);
        }
      });
    });
  }

  return out;
}
