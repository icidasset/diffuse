import foundation from "~/common/facets/foundation.js";

const queue = foundation.engine.queue();
const output = foundation.orchestrator.output();

/**
 * Playlist generator
 */
function generatePlaylist() {
  const queueItems = [
    ...queue.past(),
    ...(queue.now() ? [queue.now()] : []),
    ...queue.future().filter((i) => i.manualEntry),
  ];

  const playlist = queueItems
    .map((item) => output.tracks.collection().find((t) => t.id === item?.id))
    .filter((t) => t);

  const element = document.querySelector("main ol");
  if (!element) return;

  element.innerHTML = playlist
    .map((track) =>
      `<li>
        ${track?.tags?.artist ?? "Unknown artist"} -
        ${track?.tags?.title ?? "Unknown title"}
      </li>`
    )
    .join("");
}

/** @type {HTMLButtonElement} */ (document.body.querySelector("button"))
  .onclick = () => {
    generatePlaylist();
  };
