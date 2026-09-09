import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

effect(() => {
  if (foundation.signals.engine.queue()) {
    foundation.orchestrator.autoQueue();
  }
});
