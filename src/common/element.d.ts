import type { Tunnel } from "./worker.d.ts";

export type BroadcastingStatus =
  | { leader: true; initialLeader: boolean }
  | { leader: false };

export type HtmlTagFunction = (
  strings: string[] | ArrayLike<string>,
  ...values: unknown[]
) => string;

export type ProvisionedWorkers<T extends string> = {
  [K in T]: ProvisionedWorker;
};

export type ProvisionedWorker = Worker | SharedWorker;

export type RenderArg<State = undefined> = {
  html: HtmlTagFunction;
  state: State;
};
