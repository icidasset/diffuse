import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

let initialised = false;

effect(() => {
  const audio = foundation.signals.engine.audio();
  if (!audio) return;

  if (!initialised) {
    foundation.orchestrator.spectrogramAudio();
    initialised = true;
  }
});
