import "@awesome.me/webawesome/dist/components/input/input.js";

import * as TID from "@atcute/tid";
import { html } from "lit-html";

import * as Output from "~/common/output.js";
import { NAME as S3_NAME } from "~/components/output/bytes/s3/element.js";
import { SCHEME as S3_SCHEME } from "~/components/input/s3/constants.js";
import { bucketId, buildURI, parseURI } from "~/components/input/s3/common.js";
import { effect } from "~/common/signal.js";
import foundation from "~/common/foundation.js";

import { setup } from "~/facets/connect/common.js";

document.title = "Connect S3 | Diffuse";

/**
 * @import { default as WaInput } from "@awesome.me/webawesome/dist/components/input/input.js"
 * @import { Bucket } from "~/components/input/s3/types.d.ts"
 * @import { S3OutputElement } from "~/components/output/bytes/s3/types.d.ts"
 */

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

const s3Option = (await outputOrchestrator.options()).find(
  (o) => o.label === "S3",
);

const s3El = /** @type {S3OutputElement | undefined} */ (
  outputOrchestrator.root().querySelector(S3_NAME)
);

if (!s3Option) {
  throw new Error("S3 output was not enabled!");
}

const OUTPUT_S3_ID = s3Option?.id;

////////////////////////////////////////////
// UI
////////////////////////////////////////////

const { setItems, setError } = setup({
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

  onOutputActivate: async () => {
    await outputOrchestrator.select(OUTPUT_S3_ID);
  },
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
  const outputBucket = s3El?.bucket();
  const isSelectedOutput = outputOrchestrator.selected()?.id === OUTPUT_S3_ID;

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
      isSelectedOutput: isOutput && isSelectedOutput,
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
  setError(null);
  try {
    if (uri) {
      const tracks = await Output.data(outputOrchestrator.tracks);
      const detachedTracks = await inputConfigurator.detach({
        fileUriOrScheme: uri,
        tracks,
      });

      if (detachedTracks) await outputOrchestrator.tracks.save(detachedTracks);
    }

    if (isOutput) {
      await s3El?.unsetBucket();
    }
  } catch (err) {
    setError(err instanceof Error ? err.message : "Failed to remove bucket");
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
    await s3El?.setBucket(bucket);
    await outputOrchestrator.select(OUTPUT_S3_ID);
  }
}
