import defaults from "@common/constituents/default/config.js";
import { effect } from "@common/signal.js";

import ArtworkController from "@themes/blur/artwork-controller/element.js";

// Prerequisites
const aud = defaults.lazy.engine.audio();
const queue = defaults.lazy.engine.queue();

const art = defaults.lazy.processor.artwork();

const oqa = defaults.lazy.orchestrator.queueAudio();
const rso = defaults.lazy.orchestrator.repeatShuffle();

defaults.lazy.orchestrator.queueTracks();
defaults.lazy.orchestrator.repeatShuffle();

// Controller
const dac = new ArtworkController();
dac.setAttribute("artwork-processor-selector", art.selector);
dac.setAttribute("audio-engine-selector", aud.selector);
dac.setAttribute("input-selector", defaults.orchestrator.input.selector);
dac.setAttribute("queue-engine-selector", queue.selector);

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
