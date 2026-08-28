import { insertPreludes } from "../facets/prelude.js";

////////////////////////////////////////////
// DEMO
////////////////////////////////////////////

async function addSampleContent() {
  const addDemoBtn = document.querySelector("#add-sample-content");
  if (!addDemoBtn) return;

  addDemoBtn.setAttribute("disabled", "");
  addDemoBtn.innerHTML = `<span>
    <i class="ph-fill ph-hourglass-medium"></i>
    Loading dependencies
  </span>`;

  const { default: foundation } = await import("~/common/foundation.js");
  const Output = await import("~/common/output.js");

  const input = await foundation.configurator.input();
  const output = await foundation.orchestrator.output();
  const pto = await foundation.orchestrator.processTracks({
    disableWhenReady: true,
  });

  // Execute prelude
  await insertPreludes(
    await Output.data(output.facets),
  );

  /** @type {import("~/components/input/s3/element.js").CLASS | null} */
  const s3 = input.querySelector("di-s3");

  if (!s3) {
    throw new Error("S3 input not found");
  }

  addDemoBtn.innerHTML = `<span>
    <i class="ph-fill ph-hourglass-medium"></i>
    Adding source
  </span>`;

  const demo = await s3.demo();

  await output.tracks.save([
    ...(await Output.data(output.tracks)),
    demo.track,
  ]);

  addDemoBtn.innerHTML = `<span>
    <i class="ph-fill ph-hourglass-medium"></i>
    Processing source
  </span>`;

  await pto.process();

  addDemoBtn.innerHTML = `<span>
    <i class="ph-fill ph-check-fat"></i> Added
  </span>`;
}

export function setupSampleButton() {
  const addDemoBtn = document.querySelector("#add-sample-content");
  addDemoBtn?.addEventListener("click", addSampleContent);
}
