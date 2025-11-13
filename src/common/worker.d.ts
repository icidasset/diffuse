export type Announcement<T> = MRpcBaseMsg & { type: "announcement"; args: T };

/**
 * Comes from the `@mys/m-rpc` library,
 * but it is not exported. Used to identify
 * messages sent via `postMessage`.
 */
export type MRpcBaseMsg = { ns: string; name: string; key: number };

/** */
export type PortProvider = () => {
  disconnect(): void;
  port: MessagePort;
};

/** */
export type PortProviderMethod = { port: PortProvider };

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
export type ProxyProvider<
  Actions extends Record<string, (...args: any[]) => any>,
> = (workerOrPort: MessagePort | Worker) => ProxiedActions<Actions>;

/** */
export type ProxyProviderMethod<
  Actions extends Record<string, (...args: any[]) => any>,
> = { proxy: ProxyProvider<Actions> };

/** */
export type WorkerProvider = (group?: string) => Worker;

/** */
export type WorkerProviderMethod = { worker: WorkerProvider };
