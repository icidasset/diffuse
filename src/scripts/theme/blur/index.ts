import type { ManagedOutput } from "@applets/core/types";
import { applet } from "@scripts/applet/common";

////////////////////////////////////////////
// 🗂️ Applets
////////////////////////////////////////////
import type * as QueueEngine from "@applets/engine/queue/types.d.ts";

const container = document.querySelector("main");
if (!container) throw new Error("Missing container");

const labelA = "Deck A";
const labelB = "Deck B";

const configurator = {
  output: await applet<ManagedOutput>("/configurator/output"),
};

const _constituent = {
  a: applet("/constituent/blur/artwork-controller", { container, groupId: labelA }),
  b: applet("/constituent/blur/artwork-controller", { container, groupId: labelB }),
};

// TODO:
// const _orchestrator = {
//   primary: applet("/orchestrator/primary", { groupId: labelA }),
// };

// const engine = {
//   queue: {
//     a: await applet<QueueEngine.State>("/engine/queue", { groupId: labelA }),
//     b: await applet<QueueEngine.State>("/engine/queue", { groupId: labelB }),
//   },
// };

// const deckA = engine.queue.a;
// const deckB = engine.queue.b;
