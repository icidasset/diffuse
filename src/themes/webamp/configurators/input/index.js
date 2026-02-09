import foundation from "@common/constituents/foundation.js";
import InputConfigElement from "@themes/webamp/configurators/input/element.js";
import { effect } from "@common/signal.js";

const inp = foundation.orchestrator.input();
const out = foundation.orchestrator.output();
const pro = foundation.orchestrator.processTracks({ disableWhenReady: true });
const sou = foundation.orchestrator.sources();

const el = new InputConfigElement();
el.setAttribute("input-selector", inp.selector);
el.setAttribute("output-selector", out.selector);
el.setAttribute("sources-orchestrator-selector", sou.selector);

document.querySelector("#placeholder")?.replaceWith(el);

// EFFECTS

let initEffect = false;

effect(() => {
  const _trigger = sou.sources();
  if (out.tracks.state() !== "loaded") return;
  if (initEffect) pro.process();
  initEffect = true;
});
