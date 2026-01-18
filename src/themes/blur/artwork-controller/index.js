import defaults from "@common/constituents/default/config.js";
import { effect } from "@common/signal.js";

import AudioEngine from "@components/engine/audio/element.js";
import ArtworkProcessor from "@components/processor/artwork/element.js";
import QueueAudioOrchestrator from "@components/orchestrator/queue-audio/element.js";

import ArtworkController from "@themes/blur/artwork-controller/element.js";

// Prerequisites
const aud = new AudioEngine();
aud.setAttribute("group", defaults.GROUP);

const art = new ArtworkProcessor();
const oqa = new QueueAudioOrchestrator();
oqa.setAttribute("group", defaults.GROUP);
oqa.setAttribute("input-selector", defaults.orchestrator.input.selector);
oqa.setAttribute("audio-engine-selector", "de-audio");
oqa.setAttribute("queue-engine-selector", defaults.engine.queue.selector);

// Controller
const dac = new ArtworkController();
dac.setAttribute("artwork-processor-selector", art.selector);
dac.setAttribute("audio-engine-selector", aud.selector);
dac.setAttribute("input-selector", defaults.orchestrator.input.selector);
dac.setAttribute("queue-engine-selector", defaults.engine.queue.selector);

// Add to DOM
document.body.append(aud, art, oqa, dac);

// Effect - Link the repeat/shuffle & queue-audio orchestrators
effect(() => {
  const rso = defaults.orchestrator.repeatShuffle;
  const repeat = rso.repeat();

  if (repeat && !oqa.hasAttribute("repeat")) {
    oqa.toggleAttribute("repeat");
  } else if (!repeat && oqa.hasAttribute("repeat")) {
    oqa.removeAttribute("repeat");
  }
});
