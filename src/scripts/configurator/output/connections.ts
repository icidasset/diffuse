import type { Applet } from "@web-applets/sdk";

import type { ManagedOutput } from "@applets/core/types";
import type { Method } from "./types";
import { CONNECTIONS, CUSTOM_KEY } from "./constants";
import { applet } from "@scripts/applet/common";

const connections: Record<string, Applet<ManagedOutput>> = {};

export async function connection(method: Method) {
  if (connections[method]) return connections[method];

  let href;

  if (method === "custom") {
    href = localStorage.getItem(CUSTOM_KEY);
    if (!href) throw new Error("Missing custom applet URL");
  } else {
    href = CONNECTIONS[method];
    if (!href) throw new Error("No href defined for this connection method.");
  }

  connections[method] = await applet(href);
  return connections[method];
}
