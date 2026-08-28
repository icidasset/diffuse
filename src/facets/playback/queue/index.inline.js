import { html, render as litRender } from "lit-html";
import { keyed } from "~/vendor/lit-html/directives/keyed.js";

import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

/**
 * @import { Track } from "~/definitions/types.d.ts"
 */

foundation.setup({ title: "Queue | Diffuse" });

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

const [queue, outputOrchestrator] = await Promise.all([
  foundation.engine.queue(),
  foundation.orchestrator.output(),
]);

await Promise.all([
  customElements.whenDefined(queue.localName),
  customElements.whenDefined(outputOrchestrator.localName),
]);

////////////////////////////////////////////
// ELEMENTS
////////////////////////////////////////////

const nowSection =
  /** @type {HTMLElement} */ (document.querySelector("#now-section"));
const nowList =
  /** @type {HTMLElement} */ (document.querySelector("#now-list"));
const futureSection =
  /** @type {HTMLElement} */ (document.querySelector("#future-section"));
const futureList =
  /** @type {HTMLElement} */ (document.querySelector("#future-list"));
const pastSection =
  /** @type {HTMLElement} */ (document.querySelector("#past-section"));
const pastList =
  /** @type {HTMLElement} */ (document.querySelector("#past-list"));
const queueEmpty =
  /** @type {HTMLElement} */ (document.querySelector("#queue-empty"));

////////////////////////////////////////////
// HELPERS
////////////////////////////////////////////

/**
 * @param {string} id
 * @param {Track[]} tracks
 */
const findTrack = (id, tracks) => tracks.find((t) => t.id === id);

/**
 * @param {Track | undefined} track
 * @param {string} fallbackId
 */
const trackTitle = (track, fallbackId) => track?.tags?.title ?? fallbackId;

/**
 * @param {Track | undefined} track
 */
const trackArtist = (track) =>
  track?.tags?.artist ?? track?.tags?.albumartist ?? null;

////////////////////////////////////////////
// RENDER
////////////////////////////////////////////

effect(() => {
  const now = queue.now();
  const past = queue.past();
  const future = queue.future();

  const tracksCol = outputOrchestrator.tracks.collection();
  const tracks = tracksCol.state === "loaded" ? tracksCol.data : [];

  const hasAnything = now !== null || past.length > 0 || future.length > 0;
  queueEmpty.hidden = hasAnything;

  // Now playing
  nowSection.hidden = now === null;
  if (now !== null) {
    const track = findTrack(now.id, tracks);
    litRender(
      html`
        <li class="queue-item">
          <div class="queue-item__info">
            <span class="queue-item__title queue-item__title--now">${trackTitle(
              track,
              now.id,
            )}</span>
            ${trackArtist(track)
              ? html`
                <span class="queue-item__detail">${trackArtist(track)}</span>
              `
              : null}
          </div>
        </li>
      `,
      nowList,
    );
  }

  // Up next — move() operates on the flat [past..., now, future...] list
  futureSection.hidden = future.length === 0;
  if (future.length > 0) {
    const offset = past.length + (now !== null ? 1 : 0);

    litRender(
      html`
        ${future.map((item, i) => {
          const track = findTrack(item.id, tracks);
          return keyed(item.id, html`
            <li class="queue-item">
              <div class="queue-item__info">
                <span class="queue-item__title">${trackTitle(
                  track,
                  item.id,
                )}</span>
                ${trackArtist(track)
                  ? html`
                    <span class="queue-item__detail">${trackArtist(
                      track,
                    )}</span>
                  `
                  : null}
              </div>
              ${item.manualEntry
                ? html`
                  <span class="queue-item__badge" title="Manually queued">M</span>
                `
                : null}
              <div class="queue-item__reorder">
                <button
                  class="button--plain button--icon"
                  title="Move up"
                  ?disabled="${i === 0}"
                  @click="${() =>
                    queue.move({ key: item.key, to: offset + i - 1 })}"
                >
                  <i class="ph-bold ph-arrow-up"></i>
                </button>
                <button
                  class="button--plain button--icon"
                  title="Move down"
                  ?disabled="${i === future.length - 1}"
                  @click="${() =>
                    queue.move({ key: item.key, to: offset + i + 1 })}"
                >
                  <i class="ph-bold ph-arrow-down"></i>
                </button>
              </div>
            </li>
          `);
        })}
      `,
      futureList,
    );
  }

  // History — most recent first
  pastSection.hidden = past.length === 0;
  if (past.length > 0) {
    const reversed = [...past].reverse();
    litRender(
      html`
        ${reversed.map((item) => {
          const track = findTrack(item.id, tracks);
          return keyed(item.id, html`
            <li class="queue-item queue-item--past">
              <div class="queue-item__info">
                <span class="queue-item__title">${trackTitle(
                  track,
                  item.id,
                )}</span>
                ${trackArtist(track)
                  ? html`
                    <span class="queue-item__detail">${trackArtist(
                      track,
                    )}</span>
                  `
                  : null}
              </div>
            </li>
          `);
        })}
      `,
      pastList,
    );
  }
});

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

document.querySelector("#clear-all-btn")?.addEventListener("click", () => {
  queue.clear({ keepManual: false });
});

document.querySelector("#clear-auto-btn")?.addEventListener("click", () => {
  queue.clear({ keepManual: true });
});

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
