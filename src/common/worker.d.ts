export type Announcement<T> = MRpcBaseMsg & { type: "announcement"; args: T };
export type IncompleteArray<T> = ["Missing required items", T];

/**
 * Comes from the `@mys/m-rpc` library,
 * but it is not exported. Used to identify
 * messages sent via `postMessage`.
 */
export type MRpcBaseMsg = { ns: string; name: string; key: number };

/** */
export type ProxiedActions<
  Actions extends Record<string, (...args: any[]) => any>,
> = {
  [A in keyof Actions]: ProxiedAction<Actions[A]>;
};

export type ProxiedAction<
  Action extends (...args: any[]) => any,
  PromisedReturn =
    (ReturnType<Action> extends Promise<unknown> ? ReturnType<Action>
      : Promise<ReturnType<Action>>),
> = (...args: Parameters<Action>) => PromisedReturn;

/** */
export interface MessengerRealm {
  postMessage: MessagePort["postMessage"];
  addEventListener: MessagePort["addEventListener"];
  removeEventListener: MessagePort["removeEventListener"];
}
