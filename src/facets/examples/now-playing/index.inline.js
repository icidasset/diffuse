import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

// Set doc title
foundation.setup({ title: "Now Playing | Diffuse" });

const output = await foundation.orchestrator.output();
const queue = await foundation.engine.queue();

effect(() => {
  const now = queue.now();
  const tracksCol = output.tracks.collection();
  const isLoadingTracks = tracksCol.state !== "loaded";
  const currentlyPlaying = now && tracksCol.state === "loaded"
    ? tracksCol.data.find((t) => t.id === now.id)
    : undefined;
  const tags = currentlyPlaying?.tags;

  const element =
    /** @type {HTMLElement | null} */ (document.querySelector("#now-playing"));
  if (!element) return;

  if (currentlyPlaying) {
    element.innerText = `${tags?.artist ?? "Unknown artist"} - ${
      tags?.title ?? "Unknown title"
    }`;
  } else if (isLoadingTracks) {
    // Keep original text
  } else {
    element.innerText = "Nothing is playing yet";
  }
});

/** @type {HTMLButtonElement} */ (document.body.querySelector("button"))
  .onclick = () => {
    queue.shift();
  };
