import type {InputActions} from "~/components/input/types.d.ts"

export type Actions = InputActions & {
  cache(uris: string[]): Promise<void>
  cacheBlob(blob: Blob): Promise<string>
  listCached(): Promise<string[]>
  removeFromCache(uris: string[]): Promise<void>
};
