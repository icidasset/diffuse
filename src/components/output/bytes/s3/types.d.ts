import type { OutputElement } from "../../types.d.ts";
import type { Bucket } from "@components/input/s3/types.d.ts";

export type S3OutputElement =
  & OutputElement<Uint8Array | undefined>
  & {
    setBucket(bucket: Bucket): void;
  };

export type S3OutputWorkerActions = {
  get(args: { bucket: Bucket; name: string }): Promise<Uint8Array | undefined>;
  put(args: {
    bucket: Bucket;
    data: Uint8Array;
    name: string;
  }): Promise<void>;
};
