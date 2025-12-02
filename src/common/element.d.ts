import type { Tunnel } from "./worker.d.ts";

export type BroadcastingStatus =
  | { leader: true; initialLeader: boolean }
  | { leader: false };

export type HtmlTagFunction = (
  strings: string[] | ArrayLike<string>,
  ...values: unknown[]
) => string;

export type RenderArg<State = undefined> = {
  html: HtmlTagFunction;
  state: State;
};
