import defaults from "@common/constituents/default/config.js";
import { effect } from "@common/signal.js";

import ArtworkController from "@themes/blur/artwork-controller/element.js";

// Prerequisites
const aud = defaults.lazy.engine.audio();
const art = defaults.lazy.processor.artwork();
const oqa = defaults.lazy.orchestrator.queueAudio();

// Controller
const dac = new ArtworkController();
dac.setAttribute("artwork-processor-selector", art.selector);
dac.setAttribute("audio-engine-selector", aud.selector);
dac.setAttribute(
  "input-selector",
  defaults.instantiated.orchestrator.input.selector,
);
dac.setAttribute(
  "queue-engine-selector",
  defaults.instantiated.engine.queue.selector,
);
dac.setAttribute(
  "repeat-shuffle-orchestrator-selector",
  defaults.instantiated.orchestrator.repeatShuffle.selector,
);

// Add to DOM
document.body.append(dac);

// Effect - Link the repeat/shuffle & queue-audio orchestrators
effect(() => {
  const repeat = rso.repeat();

  if (repeat && !oqa.hasAttribute("repeat")) {
    oqa.toggleAttribute("repeat");
  } else if (!repeat && oqa.hasAttribute("repeat")) {
    oqa.removeAttribute("repeat");
  }
});
