import * as TID from "@atcute/tid";
import { html } from "lit-html";

import * as Output from "~/common/output.js";
import { NAME as S3_NAME } from "~/components/output/bytes/s3/element.js";
import { SCHEME as S3_SCHEME } from "~/components/input/s3/constants.js";
import { bucketId, buildURI, parseURI } from "~/components/input/s3/common.js";
import { effect } from "~/common/signal.js";
import foundation from "~/common/foundation.js";

import { setup } from "~/facets/connect/common.js";

foundation.setup({ title: "Connect S3 | Diffuse" });

/**
 * @import { Bucket } from "@specs/components/input/s3/types.d.ts"
 * @import { S3OutputElement } from "@specs/components/output/bytes/s3/types.d.ts"
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
    <p class="caption">
      A custom syncing strategy is used for the user-data storage, tracking what was
      added and removed so conflicts can be resolved.
    </p>
  `,

  formFields: html`
    <label>Access key <input id="s3-access-key" required></label>
    <label>Secret key <input id="s3-secret-key" type="password" required></label>
    <label>Bucket name <input id="s3-bucket-name" placeholder="my-bucket" required></label>
    <label>Host <input id="s3-host" placeholder="s3.amazonaws.com"></label>
    <label>Region <input id="s3-region" placeholder="us-east-1"></label>
    <label>Path <input id="s3-path" placeholder="/"></label>
    <p class="caption">* Required fields</p>
  `,

  onSubmit: (mode) => addBucket(mode),

  onOutputActivate: async () => {
    await outputOrchestrator.select(OUTPUT_S3_ID);
  },
});

const accessKeyInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#s3-access-key"));
const secretKeyInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#s3-secret-key"));
const bucketNameInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#s3-bucket-name"));
const hostInput = /** @type {HTMLInputElement} */ (document.querySelector("#s3-host"));
const regionInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#s3-region"));
const pathInput = /** @type {HTMLInputElement} */ (document.querySelector("#s3-path"));

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
      isDisabled: uri ? sourcesOrchestrator.isDisabled(uri) : false,
      onRemove: () => removeBucket(uri, isOutput),
      onToggleDisabled: uri ? () => sourcesOrchestrator.toggle(uri) : undefined,
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
      await outputOrchestrator.deselect();
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

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
