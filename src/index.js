import { GROUP } from "~/common/facets/foundation.js";
import * as Output from "~/common/output.js";

import InputConfigurator from "~/components/configurator/input/element.js";
import MetadataProcessor from "~/components/processor/metadata/element.js";
import OutputOrchestrator from "~/components/orchestrator/output/element.js";
import ProcessTracksOrchestrator from "~/components/orchestrator/process-tracks/element.js";
import S3Input from "~/components/input/s3/element.js";

// Add components to DOM
const s3 = new S3Input();
const input = new InputConfigurator();
input.setAttribute("group", GROUP);
input.append(s3);

const output = new OutputOrchestrator();
output.setAttribute("group", GROUP);

const metadataProcessor = new MetadataProcessor();
metadataProcessor.setAttribute("group", GROUP);

document.body.append(input, output, metadataProcessor);

const pto = new ProcessTracksOrchestrator();
pto.setAttribute("group", GROUP);
pto.setAttribute("input-selector", input.selector);
pto.setAttribute("output-selector", output.selector);
pto.setAttribute("metadata-processor-selector", metadataProcessor.selector);

document.body.append(pto);

// Demo
const addDemoBtn = document.querySelector("#add-sample-content");

async function addSampleContent() {
  if (!addDemoBtn) return;

  addDemoBtn.setAttribute("disabled", "");
  addDemoBtn.innerHTML = `<span>
    <i class="ph-fill ph-hourglass-medium"></i>
    Adding source
  </span>`;

  const demo = await s3.demo();
  await Output.waitUntilLoaded(output.tracks);

  addDemoBtn.innerHTML = `<span>
    <i class="ph-fill ph-hourglass-medium"></i>
    Processing source
  </span>`;

  await output.tracks.save([...output.tracks.collection(), demo.track]);

  await pto.process();

  addDemoBtn.innerHTML = `<span>
    <i class="ph-fill ph-check-fat"></i> Added
  </span>`;
}

addDemoBtn?.addEventListener("click", addSampleContent);
