import { html, render as litRender } from "lit-html";

import { effect, signal } from "~/common/signal.js";
import { insertPreludes } from "~/common/facets/prelude.js";
import foundation from "~/common/foundation.js";
import * as Output from "~/common/output.js";

foundation.setup({ title: "Setup | Diffuse" });

/** @param {string} path */
const loaderHref = (path) => `./l/?path=${encodeURIComponent(path)}`;

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

const [sourcesOrchestrator, outputOrchestrator, processOrchestrator] =
  await Promise.all([
    foundation.orchestrator.sources(),
    foundation.orchestrator.output(),
    foundation.orchestrator.processTracks({ disableWhenReady: true }),
  ]);

await Promise.all([
  customElements.whenDefined(sourcesOrchestrator.localName),
  customElements.whenDefined(outputOrchestrator.localName),
]);

const right =
  /** @type {HTMLElement} */ (document.querySelector("#setup-right"));

////////////////////////////////////////////
// DEMO
////////////////////////////////////////////

const demoAdding = signal(false);

async function addDemo() {
  if (demoAdding.get()) return;
  demoAdding.set(true);

  try {
    const input = await foundation.configurator.input();
    await insertPreludes(await Output.data(outputOrchestrator.facets));

    const s3 =
      /** @type {import("~/components/input/s3/element.js").CLASS | null} */ (
        input.querySelector("di-s3")
      );
    if (!s3) throw new Error("S3 input not found");

    const demo = await s3.demo();
    await outputOrchestrator.tracks.save([
      ...(await Output.data(outputOrchestrator.tracks)),
      demo.track,
    ]);
    await processOrchestrator.process();
  } catch (err) {
    console.error(err);
  } finally {
    demoAdding.set(false);
  }
}

////////////////////////////////////////////
// UI
////////////////////////////////////////////

effect(() => {
  const sourcesRecord = sourcesOrchestrator.sources();
  const hasSources = Object.values(sourcesRecord).some((s) => s.length > 0);

  const tracksCol = outputOrchestrator.tracks.collection();
  const hasTracks = tracksCol.state === "loaded" && tracksCol.data.length > 0;

  const isProcessing = processOrchestrator.isProcessing();
  const { processed, total } = processOrchestrator.progress();
  const pct = total > 0 ? Math.round((processed / total) * 100) : null;

  const hasContent = hasSources || hasTracks;

  let content;

  if (isProcessing) {
    content = html`
      <p class="setup-desc">
        Hold on for a bit, translating audio into tracks. This might take a while; you
        can come back to this later though, progress is saved.
      </p>
      <ul class="setup-actions">
        <li>
          <span class="setup-action">
            <i class="ph-fill ph-arrows-clockwise animate-spin"></i>
            <span>${pct !== null ? `Processing (${pct}%)` : "Listing…"}</span>
          </span>
        </li>
      </ul>
    `;
  } else if (!hasContent) {
    content = html`
      <p class="setup-desc">
        The first step is to add audio. You can introduce some from a number of
        different sources.
      </p>
      <ul class="setup-actions">
        <li>
          <a class="setup-action" href="${loaderHref(
            "facets/connect/index.html",
          )}" target="_blank">
            <i class="ph-fill ph-file-audio"></i>
            <span>Add some audio</span>
          </a>
        </li>
        <li>
          <button class="setup-action" @click="${addDemo}">
            <i class="ph-fill ph-flask"></i>
            <span>Or add demo content</span>
          </button>
        </li>
      </ul>
    `;
  } else {
    content = html`
      <p class="setup-desc">
        Audio was added succesfully, time to play some! Let's explore some interfaces
        to do so.
      </p>
      <ul class="setup-actions">
        <li>
          <a class="setup-action" href="./featured/?filter=interface" target="_blank">
            <i class="ph-fill ph-swatches"></i>
            <span>Try out interfaces</span>
          </a>
        </li>
        <li>
          <a class="setup-action" href="./featured/?filter=prelude" target="_blank">
            <i class="ph-fill ph-sparkle"></i>
            <span>Enable additional features</span>
          </a>
        </li>
      </ul>
    `;
  }

  litRender(content, right);
});

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
