import type { Track } from "~/definitions/types.d.ts";

export type Bucket = {
  accessKey: string;
  bucketName: string;
  host: string;
  path: string;
  region: string;
  secretKey: string;
};

export type Demo = {
  bucket: Bucket;
  track: Track;
};
