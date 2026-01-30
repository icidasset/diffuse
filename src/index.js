import { GROUP } from "@common/constituents/default.js";

import InputConfigurator from "@components/configurator/input/element.js";
import MetadataProcessor from "@components/processor/metadata/element.js";
import OutputOrchestrator from "@components/orchestrator/output/element.js";
import ProcessTracksOrchestrator from "@components/orchestrator/process-tracks/element.js";
import S3Input from "@components/input/s3/element.js";

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
  const tracks = output.tracks.collection();

  await output.tracks.save([...tracks, demo.track]);

  addDemoBtn.innerHTML = `<span>
    <i class="ph-fill ph-hourglass-medium"></i>
    Processing source
  </span>`;

  await pto.process();

  addDemoBtn.innerHTML = `<span>
    <i class="ph-fill ph-check-fat"></i> Added
  </span>`;
}

addDemoBtn?.addEventListener("click", addSampleContent);

// Version upgrade (only works with `diffuse-artifacts` deployments)
if (document.location.hostname.endsWith("diffuse.sh")) {
  document.querySelectorAll("#status").forEach(async (status) => {
    const versionOrCid =
      document.location.pathname.slice(1).split("/")[0]?.toLowerCase() ?? "";
    const usesCid = versionOrCid.startsWith("bafy");
    const { default: artifacts } = await import(
      `${document.location.origin}/artifacts.json`,
      { with: { type: "json" } }
    );

    // Latest is located at the end
    const lastArtifact = Object.values(artifacts).reverse()[0];

    // Check if using latest
    const isLatest = usesCid
      ? versionOrCid === lastArtifact.cid
      : versionOrCid === lastArtifact.version;

    // Remove loading animation
    status.querySelectorAll(".ph-spinner").forEach((icon) => {
      icon.parentElement?.classList.add("hidden");
      icon.parentElement?.classList.remove("animate-spin");
      icon.classList.remove("ph-spinner");
      icon.classList.add("ph-arrow-fat-lines-up");
    });

    // If using CID, append `/hash/` to href
    status.querySelectorAll(`[href="/latest/"]`).forEach((a) => {
      if (usesCid) a.setAttribute("href", "/latest/hash/");
      if (!isLatest) a.classList.remove("hidden");
    });
  });
} else {
  document.querySelectorAll("#status").forEach((status) => {
    status.remove();
  });
}
