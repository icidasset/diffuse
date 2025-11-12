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
  K extends keyof Actions = keyof Actions,
  FR = (ReturnType<Actions[K]> extends Promise<unknown> ? ReturnType<Actions[K]>
    : Promise<ReturnType<Actions[K]>>),
  F = (...args: Parameters<Actions[K]>) => FR,
  R = Record<keyof Actions, F>,
> = R;

/** */
export type ProxyProvider<
  Actions extends Record<string, (...args: any[]) => any>,
> = (workerOrPort: MessagePort | Worker) => ProxiedActions<Actions>;

/** */
export type ProxyProviderMethod<
  Actions extends Record<string, (...args: any[]) => any>,
> = { proxy: ProxyProvider<Actions> };
