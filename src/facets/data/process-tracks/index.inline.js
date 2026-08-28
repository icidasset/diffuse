import * as Output from "~/common/output.js";
import { html, render as litRender } from "lit-html";
import { effect } from "~/common/signal.js";
import foundation from "~/common/foundation.js";

foundation.setup({ title: "Process Tracks | Diffuse" });

const main = /** @type {HTMLElement} */ (document.querySelector("main"));

const orchestrator = await foundation.orchestrator.processTracks({
  disableWhenReady: true,
});

const placeholder = document.querySelector("#placeholder");
if (!placeholder) throw new Error("No #placeholder element");

/**
 * Wait until tracks are loaded and only then start processing.
 */
async function process() {
  const output = await foundation.orchestrator.output();

  await Output.data(output.tracks);
  await orchestrator.process();
}

effect(() => {
  const isProcessing = orchestrator.isProcessing();
  const { processed, total } = orchestrator.progress();
  const percentage = total > 0 ? Math.round((processed / total) * 100) : 0;

  litRender(
    isProcessing
      ? html`
        <fieldset>
          <legend>Processing tracks</legend>
          <div class="with-icon with-icon--large">
            <img src="images/icons/windows_98/gears-0.png" width="24" />
            <span>
              ${total === 0
                ? "Going through all the inputs and gathering the tracks ..."
                : `Making sure each track has metadata & statistics (${processed} / ${total}) ...`}
            </span>
          </div>
          <div
            class="progress-indicator"
            style="margin-top: var(--grouped-element-spacing);"
          >
            <div
              class="progress-indicator-bar"
              style="width: ${percentage}%"
            >
            </div>
          </div>
        </fieldset>
      `
      : html`
        <fieldset>
          <div class="with-icon with-icon--large">
            <img src="images/icons/windows_98/gears-0.png" width="24" />
            <div>
              <p>
                Go through all the inputs you've added and get the last audio files and
                their metadata.
              </p>
              <p>
                <button @click="${process}">
                  Process sources
                </button>
              </p>
            </div>
          </div>
        </fieldset>
      `,
    /** @type {HTMLElement} */ (placeholder),
  );
});

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
