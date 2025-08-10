import type { AppletEvent } from "@web-applets/sdk";
import { context } from "./context";

export function setContextData(event: AppletEvent) {
  context.data = event.data;
}
