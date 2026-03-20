import "@awesome.me/webawesome/dist/components/badge/badge.js";
import "@awesome.me/webawesome/dist/components/button/button.js";
import "@awesome.me/webawesome/dist/components/card/card.js";
import "@awesome.me/webawesome/dist/components/dialog/dialog.js";
import "@awesome.me/webawesome/dist/components/divider/divider.js";
import "@awesome.me/webawesome/dist/components/icon/icon.js";
import "@awesome.me/webawesome/dist/components/input/input.js";

import "~/common/webawesome/detect-dark.js";
import "~/common/webawesome/phosphor/bold.js";

import { html, nothing, render } from "lit-html";

import * as IDB from "idb-keyval";
import * as TID from "@atcute/tid";

import { bucketId, buildURI, parseURI } from "~/components/input/s3/common.js";
import { SCHEME as S3_SCHEME } from "~/components/input/s3/constants.js";
import foundation from "~/common/foundation.js";
import { effect, signal } from "~/common/signal.js";

document.title = "Connect S3 | Diffuse";

/**
 * @import { default as WaDialog } from "@awesome.me/webawesome/dist/components/dialog/dialog.js"
 * @import { default as WaInput } from "@awesome.me/webawesome/dist/components/input/input.js"
 * @import { Bucket } from "~/components/input/s3/types.d.ts"
 */

const OUTPUT_IDB_KEY = "diffuse/output/bytes/s3/bucket";

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

const [inputConfigurator, outputOrchestrator, sourcesOrchestrator] =
  await Promise.all([
    foundation.configurator.input(),
    foundation.orchestrator.output(),
    foundation.orchestrator.sources(),
  ]);

await Promise.all([
  customElements.whenDefined(inputConfigurator.localName),
  customElements.whenDefined(outputOrchestrator.localName),
  customElements.whenDefined(sourcesOrchestrator.localName),
]);

////////////////////////////////////////////
// STATE
////////////////////////////////////////////

/** @type {'input' | 'output'} */
let dialogMode = "input";

const $outputBucket = signal(
  /** @type {Bucket | undefined} */ (await IDB.get(OUTPUT_IDB_KEY)),
);

////////////////////////////////////////////
// ELEMENTS
////////////////////////////////////////////

const bucketDialog = /** @type {WaDialog} */ (
  document.querySelector("#bucket-dialog")
);

const hostInput = /** @type {WaInput} */ (document.querySelector("#host-input"));
const bucketNameInput = /** @type {WaInput} */ (
  document.querySelector("#bucket-name-input")
);
const regionInput = /** @type {WaInput} */ (
  document.querySelector("#region-input")
);
const accessKeyInput = /** @type {WaInput} */ (
  document.querySelector("#access-key-input")
);
const secretKeyInput = /** @type {WaInput} */ (
  document.querySelector("#secret-key-input")
);
const pathInput = /** @type {WaInput} */ (document.querySelector("#path-input"));

const bucketsDivider = /** @type {HTMLElement} */ (
  document.querySelector("#buckets-divider")
);

const bucketsList = /** @type {HTMLElement} */ (
  document.querySelector("#buckets-list")
);

////////////////////////////////////////////
// RENDER
////////////////////////////////////////////

effect(() => {
  const inputSources = sourcesOrchestrator.sources()[S3_SCHEME] ?? [];
  const outputBucket = $outputBucket.value;

  /** @type {Map<string, { name: string; host: string; uri?: string; isInput: boolean; isOutput: boolean }>} */
  const allBuckets = new Map();

  for (const source of inputSources) {
    const parsed = parseURI(source.uri);
    if (!parsed) continue;

    const id = bucketId(parsed.bucket);
    allBuckets.set(id, {
      name: parsed.bucket.bucketName,
      host: parsed.bucket.host,
      uri: source.uri,
      isInput: true,
      isOutput: false,
    });
  }

  if (outputBucket) {
    const id = bucketId(outputBucket);
    const existing = allBuckets.get(id);

    if (existing) {
      existing.isOutput = true;
    } else {
      allBuckets.set(id, {
        name: outputBucket.bucketName,
        host: outputBucket.host,
        uri: undefined,
        isInput: false,
        isOutput: true,
      });
    }
  }

  const hasAny = allBuckets.size > 0;
  bucketsDivider.hidden = !hasAny;
  bucketsList.hidden = !hasAny;

  render(
    html`
      ${[...allBuckets.entries()].map(
        ([id, { name, host, uri, isInput, isOutput }]) => html`
          <li class="bucket-item">
            <div class="bucket-info">
              <span class="bucket-name">${name}</span>
              <span class="bucket-host">${host}</span>
            </div>
            <div class="bucket-tags">
              ${isInput
                ? html`<wa-badge appearance="outlined" variant="brand">Input</wa-badge>`
                : nothing}
              ${isOutput
                ? html`<wa-badge appearance="outlined" variant="neutral">Output</wa-badge>`
                : nothing}
            </div>
            <wa-button
              appearance="plain"
              size="small"
              aria-label="Remove"
              @click="${() => removeBucket(uri, isOutput)}"
            >
              <wa-icon library="phosphor/bold" name="x"></wa-icon>
            </wa-button>
          </li>
        `,
      )}
    `,
    bucketsList,
  );
});

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/** @param {'input' | 'output'} mode */
function openDialog(mode) {
  dialogMode = mode;
  bucketDialog.label =
    mode === "input" ? "Add audio input" : "Use as userdata storage";
  hostInput.value = "";
  bucketNameInput.value = "";
  regionInput.value = "";
  accessKeyInput.value = "";
  secretKeyInput.value = "";
  pathInput.value = "";
  bucketDialog.open = true;
}

/**
 * @param {string | undefined} uri
 * @param {boolean} isOutput
 */
async function removeBucket(uri, isOutput) {
  if (uri) {
    const tracksCol = outputOrchestrator.tracks.collection();
    const tracks = tracksCol.state === "loaded" ? tracksCol.data : [];

    const detachedTracks = await inputConfigurator.detach({
      fileUriOrScheme: uri,
      tracks,
    });

    if (detachedTracks) await outputOrchestrator.tracks.save(detachedTracks);
  }

  if (isOutput) {
    $outputBucket.value = undefined;
    await IDB.del(OUTPUT_IDB_KEY);
  }
}

async function addBucket() {
  const host = hostInput.value?.trim();
  const bucketName = bucketNameInput.value?.trim();
  const region = regionInput.value?.trim() || "us-east-1";
  const accessKey = accessKeyInput.value?.trim();
  const secretKey = secretKeyInput.value?.trim();
  const path = pathInput.value?.trim() || "/";

  if (!host || !bucketName || !accessKey || !secretKey) return;

  /** @type {Bucket} */
  const bucket = { accessKey, bucketName, host, path, region, secretKey };

  if (dialogMode === "input") {
    const now = new Date().toISOString();
    const uri = buildURI(bucket);

    const tracksCol = outputOrchestrator.tracks.collection();
    const existingTracks = tracksCol.state === "loaded" ? tracksCol.data : [];

    await outputOrchestrator.tracks.save([
      ...existingTracks,
      {
        $type: "sh.diffuse.output.track",
        id: TID.now(),
        createdAt: now,
        updatedAt: now,
        kind: "placeholder",
        uri,
      },
    ]);
  } else {
    $outputBucket.value = bucket;
    await IDB.set(OUTPUT_IDB_KEY, bucket);

    const option = (await outputOrchestrator.options()).find(
      (o) => o.label === "S3",
    );
    if (option) await outputOrchestrator.select(option.id);
  }

  bucketDialog.open = false;
}

////////////////////////////////////////////
// EVENT LISTENERS
////////////////////////////////////////////

document
  .querySelector("#add-input-btn")
  ?.addEventListener("click", () => openDialog("input"));

document
  .querySelector("#add-output-btn")
  ?.addEventListener("click", () => openDialog("output"));

document.querySelector("#cancel-btn")?.addEventListener("click", () => {
  bucketDialog.open = false;
});

document.querySelector("#confirm-btn")?.addEventListener("click", addBucket);
