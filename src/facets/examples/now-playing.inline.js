import foundation from "@diffuse/foundation";
import { computed, effect } from "@diffuse/common/signal.js";

foundation.features.processInputs();
foundation.features.fillQueueAutomatically();

const output = foundation.orchestrator.output();
const queue = foundation.engine.queue();

const isLoadingTracks = computed(() => {
  return output.tracks.state() !== "loaded";
});

effect(() => {
  const now = queue.now();
  const currentlyPlaying = now
    ? output.tracks.collection().find((t) => t.id === now.id)
    : undefined;
  const tags = currentlyPlaying?.tags;

  const element =
    /** @type {HTMLElement | null} */ (document.querySelector("#now-playing"));
  if (!element) return;

  if (currentlyPlaying) {
    element.innerText = `${tags?.artist ?? "Unknown artist"} - ${
      tags?.title ?? "Unknown title"
    }`;
  } else if (isLoadingTracks()) {
    // Keep original text
  } else {
    element.innerText = "Nothing is playing yet";
  }
});

/** @type {HTMLButtonElement} */ (document.body.querySelector("button"))
  .onclick = () => {
    queue.shift();
  };
