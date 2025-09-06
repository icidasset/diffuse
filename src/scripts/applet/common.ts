import type { Applet, AppletEvent, AppletScope } from "@web-applets/sdk";
import * as Comlink from "comlink";

import { applets } from "@web-applets/sdk";
import QS from "query-string";

import { type ElementConfigurator, h } from "@scripts/spellcaster/hyperscript.js";
import { isSignal, type Signal, signal } from "@scripts/spellcaster";

import type { ResolvedUri } from "@applets/core/types";
import { transfer, type WorkerTasks } from "@scripts/common";

////////////////////////////////////////////
// 🪟 Applet connecting
////////////////////////////////////////////
export async function applet<D>(
  src: string,
  opts: {
    addSlashSuffix?: boolean;
    container?: HTMLElement | Element;
    context?: Window;
    frameId?: string;
    groupId?: string;
    newInstance?: boolean;
    setHeight?: boolean;
  } = {},
): Promise<Applet<D>> {
  src = `${src}${
    src.endsWith("/")
      ? ""
      : opts.addSlashSuffix === undefined || opts.addSlashSuffix === true
        ? "/"
        : ""
  }`;

  let query: undefined | Record<string, string>;
  query = { groupId: opts.groupId || "main" };

  if (query) {
    src = QS.stringifyUrl({ url: src, query });
  }

  let context = opts.newInstance ? self : opts.context || self.top || self.parent;

  let existingFrame: HTMLIFrameElement | null;

  // TODO: Ideally we do some cross-origin detection here
  try {
    existingFrame = opts.newInstance ? null : context.document.querySelector(`[src="${src}"]`);
  } catch (err) {
    existingFrame = null;
    context = self;
  }

  let frame;

  if (existingFrame) {
    frame = existingFrame;
  } else {
    frame = document.createElement("iframe");
    frame.loading = "eager";
    frame.src = src;
    if (opts.frameId) frame.id = opts.frameId;

    if (opts.container) {
      opts.container.appendChild(frame);
    } else {
      context.document.body.appendChild(frame);
    }
  }

  if (frame.contentWindow === null) {
    throw new Error("iframe does not have a contentWindow");
  }

  const applet = await applets.connect<D>(frame.contentWindow, { context }).catch((err) => {
    console.error("Error connecting to " + src, err);
    throw err;
  });

  if (opts.setHeight) {
    applet.onresize = () => {
      frame.height = `${applet.height}px`;
      frame.classList.add("has-loaded");
    };
  } else {
    if (frame.contentDocument?.readyState === "complete") {
      frame.classList.add("has-loaded");
    }

    frame.addEventListener("load", () => {
      frame.classList.add("has-loaded");
    });
  }

  return applet;
}

export function tunnel(
  worker: Comlink.Remote<WorkerTasks>,
  connections: Record<string, Applet | Promise<Applet>>,
) {
  Object.entries(connections).forEach(([scheme, promise]) => {
    Promise.resolve(promise).then((conn) => {
      return worker._manage(scheme, transfer(conn.ports.worker));
    });
  });
}

////////////////////////////////////////////
// 🪟 Applet registration
////////////////////////////////////////////
export type DiffuseApplet<T> = {
  groupId: string | undefined;
  scope: AppletScope<T>;

  settled(): Promise<void>;

  get instanceId(): string;
  set data(data: T);

  codec: Codec<T>;
  unloadHandler?: () => void;

  isMainInstance(): boolean | null;
  setActionHandler<H extends Function>(actionId: string, actionHandler: H): void;
};

export type Codec<T> = {
  decode(data: any): T;
  encode(data: T): any;
};

export function lookupGroupId() {
  const url = new URL(location.href);
  return url.searchParams.get("groupId") || "main";
}

export function register<DataType = any>(
  options: { mode?: "broadcast" | "shared-worker"; worker?: Comlink.Remote<WorkerTasks> } = {},
): DiffuseApplet<DataType> {
  const mode = options.mode ?? "broadcast";
  const scope = applets.register<DataType>();

  const groupId = lookupGroupId();
  const channelId = `${location.host}${location.pathname}/${groupId}`;
  const instanceId = crypto.randomUUID();

  // Codec
  const codec = {
    decode: (data: any) => data as DataType,
    encode: (data: DataType) => data as any,
  };

  // Context
  const context: DiffuseApplet<DataType> = {
    groupId,
    scope,

    settled() {
      return channelContext?.promise.then(() => {}) ?? Promise.resolve();
    },

    get instanceId() {
      return instanceId;
    },

    get data() {
      return scope.data;
    },

    set data(data: DataType) {
      scope.data = data;
    },

    codec,

    isMainInstance() {
      return channelContext?.mainSignal() ?? null;
    },

    setActionHandler: <H extends Function>(actionId: string, actionHandler: H) => {
      switch (mode) {
        case "broadcast":
          return channelContext?.setActionHandler(actionId, actionHandler);

        case "shared-worker":
          return scope.setActionHandler(actionId, actionHandler);
      }
    },
  };

  if (options.worker) {
    context.scope.onworkerport = (event) => {
      if (!event.port) return;
      options.worker?._listen(transfer(event.port));
    };
  }

  // Channel
  const channelContext =
    mode === "broadcast"
      ? broadcastChannel<DataType>({
          channelId,
          context,
          instanceId,
          scope,
        })
      : undefined;

  return context;
}

function broadcastChannel<DataType>({
  channelId,
  context,
  instanceId,
  scope,
}: {
  channelId: string;
  context: DiffuseApplet<DataType>;
  instanceId: string;
  scope: AppletScope<DataType>;
}) {
  const isMain = signal<boolean>(true);

  // One instance to rule them all
  //
  // Ping other instances to see if there are any.
  // As long as there aren't any, it is considered the main instance.
  //
  // Actions are performed on the main instance,
  // and data is replicated from main to the other instances.
  const channel = new BroadcastChannel(channelId);

  channel.addEventListener("message", async (event) => {
    switch (event.data?.type) {
      case "PING": {
        channel.postMessage({
          type: "PONG",
          instanceId: event.data.instanceId,
          originInstanceId: instanceId,
        });

        if (isMain() && event.data?.isInitialPing === true) {
          channel.postMessage({
            type: "data",
            data: context.codec.encode(scope.data),
          });
        }
        break;
      }

      case "PONG": {
        if (event.data.instanceId === instanceId) {
          isMain(false);
        }
        break;
      }

      case "UNLOADED": {
        if (!context.isMainInstance()) {
          // We need to wait until the other side is actually unloaded 🤷‍♀️
          setTimeout(async () => {
            const promised = await makeMainPromise();
            isMain(promised.isMain);
            if (promised.isMain) context.unloadHandler?.();
          }, 250);
        }
        break;
      }

      case "action": {
        if (isMain()) {
          const result = await scope.actionHandlers[event.data.actionId]?.(...event.data.arguments);
          channel.postMessage({
            type: "actioncomplete",
            actionInstanceId: event.data.actionInstanceId,
            result,
          });
        }
        break;
      }

      case "data": {
        scope.data = context.codec.decode(event.data.data);
        break;
      }
    }
  });

  // Promise that fullfills whenever it figures out its the main instance or not.
  let pinged = false;

  function makeMainPromise(timeoutDuration: number = 500) {
    return new Promise<{ isMain: boolean }>((resolve) => {
      const timeoutId = setTimeout(() => {
        channel.removeEventListener("message", handler);
        resolve({ isMain: true });
      }, timeoutDuration);

      const handler = (event: MessageEvent) => {
        if (
          (event.data?.type === "PONG" || event.data?.type === "PING") &&
          event.data?.instanceId === instanceId
        ) {
          clearTimeout(timeoutId);
          channel.removeEventListener("message", handler);
          resolve({ isMain: false });
        }
      };

      channel.addEventListener("message", handler);
      channel.postMessage({
        type: "PING",
        instanceId,
        isInitialPing: !pinged,
      });

      pinged = true;
    });
  }

  const promise = makeMainPromise();

  // If the data on the main instance changes,
  // pass it on to other instances.
  scope.addEventListener("data", async (event: AppletEvent) => {
    await promise;

    if (isMain()) {
      channel.postMessage({
        type: "data",
        data: context.codec.encode(event.data),
      });
    }
  });

  // Action handler
  const setActionHandler = <H extends Function>(actionId: string, actionHandler: H) => {
    const handler = async (...args: any) => {
      if (isMain()) {
        return actionHandler(...args);
      }

      // Check if a main instance is still available,
      // if not, then this is the new main.
      const promised = await makeMainPromise();
      isMain(promised.isMain);

      if (isMain()) {
        return actionHandler(...args);
      }

      const actionMessage = {
        actionInstanceId: crypto.randomUUID(),
        actionId,
        type: "action",
        arguments: args,
      };

      return await new Promise((resolve) => {
        const actionCallback = (event: MessageEvent) => {
          if (
            event.data?.type === "actioncomplete" &&
            event.data?.actionInstanceId === actionMessage.actionInstanceId
          ) {
            channel.removeEventListener("message", actionCallback);
            resolve(event.data.result);
          }
        };

        channel.addEventListener("message", actionCallback);
        channel.postMessage(actionMessage);
      });
    };

    scope.setActionHandler(actionId, handler);
  };

  // Before unload
  self.addEventListener("beforeunload", (event) => {
    if (context.isMainInstance()) {
      channel.postMessage({
        type: "UNLOADED",
      });
    }
  });

  // Fin
  return {
    channel,
    mainSignal: isMain,
    promise,
    setActionHandler,
  };
}

////////////////////////////////////////////
// 🔮 Reactive state management
////////////////////////////////////////////
export function reactive<D, T>(
  applet: Applet<D> | AppletScope<D>,
  dataFn: (data: D) => T,
  effectFn: (t: T) => void,
) {
  let value = dataFn(applet.data);
  effectFn(value);

  applet.addEventListener("data", (event: AppletEvent) => {
    const newData = dataFn(event.data);
    if (newData !== value) {
      value = newData;
      effectFn(value);
    }
  });
}

////////////////////////////////////////////
// ⚡️ COMMON ACTION CALLS
////////////////////////////////////////////

export async function inputUrl(input: Applet, uri: string, method = "GET") {
  return await input.sendAction<ResolvedUri>(
    "resolve",
    {
      method,
      uri,
    },
    {
      timeoutDuration: 60000 * 5,
      worker: true,
    },
  );
}

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////
export function addScope<O extends object>(astroScope: string, object: O): O {
  return {
    ...object,
    attrs: {
      ...((object as any).attrs || {}),
      [`data-astro-cid-${astroScope}`]: "",
    },
  };
}

export function appletScopePort() {
  let port: MessagePort | undefined;

  function connection(event: AppletEvent) {
    if (event.data?.type === "appletconnect") {
      window.removeEventListener("message", connection);
      port = (event as any).ports[0];
    }
  }

  window.addEventListener("message", connection);

  return () => port;
}

export function hs(
  tag: string,
  astroScope: string,
  props?: Record<string, unknown> | Signal<Record<string, unknown>>,
  configure?: ElementConfigurator,
) {
  const propsWithScope =
    props && isSignal(props)
      ? () => addScope(astroScope, props())
      : addScope(astroScope, props || {});

  return h(tag, propsWithScope, configure);
}

export function wait<A>(applet: Applet<A>, dataFn: (a: A | undefined) => boolean): Promise<void> {
  return new Promise((resolve) => {
    if (dataFn(applet.data) === true) {
      resolve();
      return;
    }

    const callback = (event: AppletEvent) => {
      if (dataFn(event.data) === true) {
        applet.removeEventListener("data", callback);
        resolve();
      }
    };

    applet.addEventListener("data", callback);
  });
}
