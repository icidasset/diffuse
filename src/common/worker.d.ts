export type Announcement<T> = MRpcBaseMsg & { type: "announcement"; args: T };
export type MRpcBaseMsg = { ns: string; name: string; key: number };
