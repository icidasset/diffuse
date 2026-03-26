import foundation from "~/common/foundation.js";

// Set doc title
foundation.setup({ title: "Generate Playlist | Diffuse" });

const output = await foundation.orchestrator.output();
const queue = await foundation.engine.queue();

/**
 * Playlist generator
 */
function generatePlaylist() {
  const queueItems = [
    ...queue.past(),
    ...(queue.now() ? [queue.now()] : []),
    ...queue.future().filter((i) => i.manualEntry),
  ];

  const tracksCol = output.tracks.collection();
  const tracksList = tracksCol.state === "loaded" ? tracksCol.data : [];
  const playlist = queueItems
    .map((item) => tracksList.find((t) => t.id === item?.id))
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
