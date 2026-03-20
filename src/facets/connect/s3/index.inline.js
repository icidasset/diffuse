import "@awesome.me/webawesome/dist/components/input/input.js";

import "~/common/webawesome/detect-dark.js";
import "~/common/webawesome/phosphor/bold.js";

import { html } from "lit-html";

import * as IDB from "idb-keyval";
import * as TID from "@atcute/tid";

import { bucketId, buildURI, parseURI } from "~/components/input/s3/common.js";
import { SCHEME as S3_SCHEME } from "~/components/input/s3/constants.js";
import foundation from "~/common/foundation.js";
import { effect, signal } from "~/common/signal.js";

import { setup } from "~/facets/connect/common.js";

document.title = "Connect S3 | Diffuse";

/**
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

const $outputBucket = signal(
  /** @type {Bucket | undefined} */ (await IDB.get(OUTPUT_IDB_KEY)),
);

////////////////////////////////////////////
// UI
////////////////////////////////////////////

const { setItems } = setup({
  title: "S3",

  description: html`
    <p>
      Connect to an S3-compatible storage to use it as audio input or user-data
      storage.
    </p>
    <p class="wa-caption-xs">
      A custom syncing strategy is used for the user-data storage, tracking what was
      added and removed so conflicts can be resolved.
    </p>
  `,

  formFields: html`
    <wa-input id="s3-access-key" label="Access key" required></wa-input>
    <wa-input
      id="s3-secret-key"
      label="Secret key"
      type="password"
      required
    ></wa-input>
    <wa-input
      id="s3-bucket-name"
      label="Bucket name"
      placeholder="my-bucket"
      required
    ></wa-input>
    <wa-input id="s3-host" label="Host" placeholder="s3.amazonaws.com"></wa-input>
    <wa-input id="s3-region" label="Region" placeholder="us-east-1"></wa-input>
    <wa-input id="s3-path" label="Path" placeholder="/"></wa-input>
    <p class="wa-caption-xs">* Required fields</p>
  `,

  onSubmit: (mode) => addBucket(mode),
});

const accessKeyInput =
  /** @type {WaInput} */ (document.querySelector("#s3-access-key"));
const secretKeyInput =
  /** @type {WaInput} */ (document.querySelector("#s3-secret-key"));
const bucketNameInput =
  /** @type {WaInput} */ (document.querySelector("#s3-bucket-name"));
const hostInput = /** @type {WaInput} */ (document.querySelector("#s3-host"));
const regionInput =
  /** @type {WaInput} */ (document.querySelector("#s3-region"));
const pathInput = /** @type {WaInput} */ (document.querySelector("#s3-path"));

////////////////////////////////////////////
// REACTIVE LIST
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

  setItems(
    [...allBuckets.values()].map(({ name, host, uri, isInput, isOutput }) => ({
      name,
      detail: host,
      isInput,
      isOutput,
      onRemove: () => removeBucket(uri, isOutput),
    })),
  );
});

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

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

/** @param {'input' | 'output'} mode */
async function addBucket(mode) {
  const accessKey = accessKeyInput.value?.trim();
  const secretKey = secretKeyInput.value?.trim();
  const bucketName = bucketNameInput.value?.trim();
  const rawHost = hostInput.value?.trim();
  const host = rawHost?.length
    ? rawHost.replace(/^\w+:\/\//, "")
    : "s3.amazonaws.com";
  const region = regionInput.value?.trim() || "us-east-1";
  const path = pathInput.value?.trim() || "/";

  if (!accessKey || !secretKey || !bucketName) return;

  /** @type {Bucket} */
  const bucket = { accessKey, bucketName, host, path, region, secretKey };

  if (mode === "input") {
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
}
