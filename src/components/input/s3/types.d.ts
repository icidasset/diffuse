import type { SignalReader } from "@common/signal.d.ts";

export type Bucket = {
  accessKey: string;
  bucketName: string;
  host: string;
  path: string;
  region: string;
  secretKey: string;
};
