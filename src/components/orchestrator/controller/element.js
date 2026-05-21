import { defineElement, DiffuseElement, query, whenElementsDefined } from "~/common/element.js";
import { computed, signal } from "~/common/signal.js";

/**
 * @import {OutputElement} from "@specs/components/output/types.d.ts"
 * @import AudioEngine from "~/components/engine/audio/element.js"
 * @import QueueEngine from "~/components/engine/queue/element.js"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Provides commonly used computed signals derived from the audio engine,
 * queue engine, and output — so theme elements don't need to re-implement them.
 */
class ControllerOrchestrator extends DiffuseElement {
  static NAME = "diffuse/orchestrator/controller";

  // SIGNALS - DEPENDENCIES

  $audio = signal(/** @type {AudioEngine | undefined} */ (undefined));
  $output = signal(/** @type {OutputElement | undefined} */ (undefined));
  $queue = signal(/** @type {QueueEngine | undefined} */ (undefined));

  // SIGNALS - COMPUTED

  audio = computed(() => {
    const curr = this.$queue.value?.now();
    return curr ? this.$audio.value?.state(curr.id) : undefined;
  });

  currentTrack = computed(() => {
    const item = this.$queue.value?.now();
    if (!item) return undefined;
    const col = this.$output.value?.tracks.collection();
    if (!col || col.state !== "loaded") return undefined;
    return col.data.find((t) => t.id === item.id);
  });

  isPlaying = computed(() => {
    return this.$audio.value?.isPlaying();
  });

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    /** @type {AudioEngine} */
    const audio = query(this, "audio-engine-selector");

    /** @type {OutputElement} */
    const output = query(this, "output-selector");

    /** @type {QueueEngine} */
    const queue = query(this, "queue-engine-selector");

    whenElementsDefined({ audio, output, queue }).then(() => {
      this.$audio.value = audio;
      this.$output.value = output;
      this.$queue.value = queue;
    });
  }
}

export default ControllerOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ControllerOrchestrator;
export const NAME = "do-controller";

defineElement(NAME, CLASS);
