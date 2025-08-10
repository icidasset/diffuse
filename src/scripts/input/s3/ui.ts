import { computed, effect, type Signal, signal } from "spellcaster";
import { type Props, repeat, tags, text } from "spellcaster/hyperscript.js";

import type { Bucket } from "./types";
import { bucketId, loadBuckets, saveBuckets } from "./common";

////////////////////////////////////////////
// UI
////////////////////////////////////////////
export const [buckets, setBuckets] = signal<Record<string, Bucket>>(await loadBuckets());
export const [form, setForm] = signal<{
  access_key?: string;
  bucket_name?: string;
  host?: string;
  path?: string;
  region?: string;
  secret_key?: string;
}>({});

export const bucketsMap = computed(() => {
  return new Map(Object.entries(buckets()));
});

effect(() => {
  saveBuckets(buckets());
});

////////////////////////////////////////////
// UI ~ BUCKETS
////////////////////////////////////////////
const Bucket = (bucket: Signal<Bucket>) => {
  const onclick = () => {
    const b = bucket();
    const id = bucketId(b);

    const col = { ...buckets() };
    delete col[id];

    setBuckets(col);
  };

  return tags.li({ onclick, style: "cursor: pointer" }, text(bucket().host));
};

const BucketList = computed(() => {
  if (bucketsMap().size === 0) {
    return tags.p({ id: "buckets" }, [tags.small({}, text("Nothing added so far."))]);
  }

  return tags.ul({ id: "buckets" }, repeat(bucketsMap, Bucket));
});

effect(() => {
  document.querySelector("#buckets")?.replaceWith(BucketList());
});

////////////////////////////////////////////
// UI ~ FORM
////////////////////////////////////////////
function addBucket(event: Event) {
  event.preventDefault();

  const f = form();

  const bucket: Bucket = {
    accessKey: f.access_key || "",
    bucketName: f.bucket_name || "",
    host: f.host || "s3.amazonaws.com",
    path: f.path || "/",
    region: f.region || "us-east-1",
    secretKey: f.secret_key || "",
  };

  setBuckets({
    ...buckets(),
    [bucketId(bucket)]: bucket,
  });
}

function Form() {
  return tags.form({ onsubmit: addBucket }, [
    tags.fieldset({ className: "grid" }, [
      Input("access_key", "Access key", "r31w7m9c", { required: true }),
      Input("secret_key", "Secret key", "v02g2l29", { required: true }),
    ]),
    tags.fieldset({ className: "grid" }, [
      Input("bucket_name", "Bucket name", "bucket", { required: true }),
      Input("region", "Region", "us-east-1", { required: true }),
    ]),
    tags.fieldset({ className: "grid" }, [
      Input("host", "Host", "s3.amazonaws.com", { required: true }),
      Input("path", "Path", "/"),
    ]),
    tags.fieldset({ className: "grid" }, [tags.input({ type: "submit", value: "Connect" }, [])]),
  ]);
}

function Input(name: string, label: string, placeholder: string, opts: Props = {}) {
  return tags.label({}, [
    tags.span({}, [
      tags.span({}, text(label)),
      tags.small({}, text("required" in opts ? "" : " (optional)")),
    ]),
    tags.input({
      ...opts,
      name,
      placeholder,
      oninput: (event: InputEvent) => formInput(name, (event.target as HTMLInputElement).value),
    }),
  ]);
}

function formInput(name: string, value: string) {
  setForm({ ...form(), [name]: value });
}

// 🚀
document.querySelector("#form")?.replaceWith(Form());
