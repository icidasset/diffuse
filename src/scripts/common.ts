import * as Uint8 from "uint8arrays";
import * as Comlink from "comlink";
import { xxh32 } from "xxh32";
import { getTransferables } from "@okikio/transferables";

import type { Track } from "@applets/core/types";
import type { DiffuseApplet } from "./applet/common";

// export { SharedWorkerPolyfill as SharedWorker } from "@okikio/sharedworker";
export const SharedWorker = globalThis.SharedWorker;

////////////////////////////////////////////
// 🌳
////////////////////////////////////////////

export type WorkerTasks = {
  _listen: ReturnType<typeof _listen>;
  _manage: ReturnType<typeof _manage>;
};

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////

export function arrayShuffle<T>(array: Array<T>): Array<T> {
  if (array.length === 0) {
    return [];
  }

  array = [...array];

  for (let index = array.length - 1; index > 0; index--) {
    const randArr = crypto.getRandomValues(new Uint32Array(1));
    const randVal = randArr[0] / 2 ** 32;
    const newIndex = Math.floor(randVal * (index + 1));
    [array[index], array[newIndex]] = [array[newIndex], array[index]];
  }

  return array;
}

export function cleanUndefinedValuesForTracks(tracks: Track[]): Track[] {
  return tracks.map((track) => {
    const t = { ...track };

    if (t.tags) {
      if ("album" in t.tags && t.tags.album === undefined) delete t.tags.album;
      if ("artist" in t.tags && t.tags.artist === undefined) delete t.tags.artist;
      if ("genre" in t.tags && t.tags.genre === undefined) delete t.tags.genre;
      if ("year" in t.tags && t.tags.year === undefined) delete t.tags.year;

      if ("of" in t.tags.disc && t.tags.disc.of === undefined) delete t.tags.disc.of;
      if ("of" in t.tags.track && t.tags.track.of === undefined) delete t.tags.track.of;
    }

    return t;
  });
}

export function comparable(value: unknown) {
  return xxh32(JSON.stringify(value));
}

export function endpoint<T extends Record<string, any> = WorkerTasks>(ini: Comlink.Endpoint) {
  const e = Comlink.wrap<T>(ini);
  if ("start" in ini && typeof ini.start === "function") ini.start();
  return e;
}

export function expose<A extends Record<string, any>>(
  tasks: A,
  opts?: {
    ports?: {
      applets: MessagePort[];
      consumers: MessagePort[];
    };
  },
): A {
  if (globalThis.SharedWorkerGlobalScope && self instanceof SharedWorkerGlobalScope) {
    self.onconnect = (event: MessageEvent) => {
      const port = event.ports[0];
      opts?.ports?.applets?.push(port);
      Comlink.expose(tasks, port);
      port.start();
    };

    (self as any).connected = true;
  } else {
    Comlink.expose(tasks, self);
  }

  return tasks;
}

export function groupTracksPerScheme(
  tracks: Track[],
  initial: Record<string, Track[]> = {},
): Record<string, Track[]> {
  const acc: Record<string, Track[]> = initial;

  tracks.forEach((track) => {
    const scheme = track.uri.substring(0, track.uri.indexOf(":"));
    acc[scheme] ??= [];
    acc[scheme].push(track);
  });

  return acc;
}

export function inIframe() {
  return window.self !== window.top;
}

export function initialConnections<C extends Record<string, any>>(ids: string[]) {
  const connections: Record<string, PromiseWithResolvers<Comlink.Remote<C>>> = {};

  ids.forEach((c) => {
    connections[c] = Promise.withResolvers<Comlink.Remote<C>>();
  });

  return connections;
}

export function isPrimitive(test: unknown) {
  return test !== Object(test);
}

export function jsonDecode<T>(a: any): T {
  return JSON.parse(new TextDecoder().decode(a));
}

export function jsonEncode<T>(a: T): Uint8Array {
  return new TextEncoder().encode(JSON.stringify(a));
}

export function postMessages<D, T>({
  data,
  ports,
  transfer,
}: {
  data: D;
  ports: MessagePort[];
  transfer?: Transferable[];
}) {
  ports.forEach((port) => {
    port.postMessage(data, transfer ?? []);
  });
}

export function provide<
  C extends Record<string, any>,
  A extends Record<string, any>,
  T extends Record<string, any>,
>({
  actions,
  connections,
  tasks,
}: {
  actions?: A;
  connections?: Record<string, PromiseWithResolvers<Comlink.Remote<C>>>;
  tasks?: T;
}) {
  const portsHolder = {
    applets: [] as MessagePort[],
    consumers: [] as MessagePort[],
  };

  const allTasks = expose<WorkerTasks & T>(
    {
      _listen: _listen<A>(actions || ({} as A), portsHolder),
      _manage: _manage<C>(connections || {}),
      ...(tasks || ({} as T)),
    },
    {
      ports: portsHolder,
    },
  );

  return {
    connections: connections || ({} as Record<string, PromiseWithResolvers<Comlink.Remote<C>>>),
    ports: portsHolder,
    tasks: allTasks,
  };
}

export function sync<DataType = unknown>(
  context: DiffuseApplet<DataType>,
  port: MessagePort | Worker,
  options: { groupId?: string } = {},
) {
  port.onmessage = (event) => {
    if (
      event.data?.type === "data" &&
      (options.groupId ? event.data?.groupId === options.groupId : true)
    ) {
      context.data = event.data.data;
    }
  };
}

export async function trackArtworkCacheId(track: Track): Promise<string> {
  return await crypto.subtle
    .digest("SHA-256", new TextEncoder().encode(track.uri))
    .then((a) => Uint8.toString(new Uint8Array(a), "base64url"));
}

export function transfer<T = unknown>(a: T) {
  const b = getTransferables(a);
  return Comlink.transfer(a, b);
}

// PRIVATE

function _listen<A extends Record<string, any>>(
  actions: A,
  portsHolder: {
    applets: MessagePort[];
    consumers: MessagePort[];
  },
) {
  async function handleAction(
    port: MessagePort,
    action: {
      type: "action";
      id: string;
      actionId: string;
      arguments: any;
    },
  ) {
    const result = await actions[action.actionId]?.(action.arguments);
    return postMessage(port, action.id, result);
  }

  function postMessage<T>(port: MessagePort, id: string, result: T) {
    port.postMessage(
      {
        type: "actioncomplete",
        id,
        result,
      },
      {
        transfer: getTransferables(result),
      },
    );
  }

  return (port: MessagePort) => {
    Comlink.expose(actions, port);
    portsHolder.consumers.push(port);

    port.onmessage = async (message) => {
      switch (message.data?.type) {
        case "action":
          return handleAction(port, message.data);
      }
    };
  };
}

function _manage<C extends Record<string, any>>(
  connections: Record<string, PromiseWithResolvers<Comlink.Remote<C>>>,
) {
  return (connectionId: string, workerPort: MessagePort) => {
    let conn = connections[connectionId];
    const remote = endpoint<C>(workerPort);

    if (!conn) {
      connections[connectionId] = Promise.withResolvers<Comlink.Remote<C>>();
      conn = connections[connectionId];
    }

    conn.resolve(remote);
  };
}
