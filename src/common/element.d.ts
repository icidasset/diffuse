export type BroadcastingStatus =
  | { leader: true; initialLeader: boolean }
  | { leader: false };

export type FnParams<Fn> = Fn extends (...args: infer P) => any ? P : never;
export type FnReturn<Fn> = Fn extends (...args: any[]) => infer P ? P : never;

export type HtmlTagFunction = (
  strings: string[] | ArrayLike<string>,
  ...values: unknown[]
) => string;

export type RenderArg<State = undefined> = {
  html: HtmlTagFunction;
  state: State;
};
