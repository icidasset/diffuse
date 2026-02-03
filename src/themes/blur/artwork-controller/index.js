import foundation from "@common/constituents/foundation.js";
import { effect } from "@common/signal.js";

import ArtworkController from "@themes/blur/artwork-controller/element.js";

// Setup the prerequisite elements
const assemblage = foundation.assemblage.playAudioFromQueue();

const aud = assemblage.engine.audio;
const inp = assemblage.orchestrator.input;
const oqa = assemblage.orchestrator.queueAudio;
const ors = assemblage.orchestrator.repeatShuffle;
const que = assemblage.engine.queue;

const art = foundation.processor.artwork();

// Controller
const dac = new ArtworkController();
dac.setAttribute("artwork-processor-selector", art.selector);
dac.setAttribute("audio-engine-selector", aud.selector);
dac.setAttribute("input-selector", inp.selector);
dac.setAttribute("queue-engine-selector", que.selector);
dac.setAttribute("repeat-shuffle-orchestrator-selector", ors.selector);

// Add to DOM
document.body.append(dac);

// Effect - Link the repeat/shuffle & queue-audio orchestrators
effect(() => {
  const repeat = ors.repeat();

  if (repeat && !oqa.hasAttribute("repeat")) {
    oqa.toggleAttribute("repeat");
  } else if (!repeat && oqa.hasAttribute("repeat")) {
    oqa.removeAttribute("repeat");
  }
});
