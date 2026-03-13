import foundation from "~/common/facets/foundation.js";
import { effect } from "~/common/signal.js";

effect(() => {
  if (foundation.signals.engine.audio()) {
    foundation.orchestrator.scrobbleAudio();
  }
});
