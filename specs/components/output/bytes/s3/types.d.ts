import type { SignalReader } from "~/common/signal.d.ts";
import type { OutputElement } from "@specs/components/output/types.d.ts";
import type { Bucket } from "@specs/components/input/s3/types.d.ts";

export type S3OutputElement =
  & OutputElement<Uint8Array | undefined>
  & {
    bucket: SignalReader<Bucket | undefined>;

    getBucket(): Promise<Bucket | undefined>;
    setBucket(bucket: Bucket): Promise<void>;
    unsetBucket(): Promise<void>;
  };

export type S3OutputWorkerActions = {
  get(args: { bucket: Bucket; name: string }): Promise<Uint8Array | undefined>;
  put(args: {
    bucket: Bucket;
    data: Uint8Array;
    name: string;
  }): Promise<void>;
};
