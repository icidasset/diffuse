import { BroadcastableDiffuseElement, query } from "@common/element.js";
import { signal } from "@common/signal.js";

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class RepeatShuffleOrchestrator extends BroadcastableDiffuseElement {
  static NAME = "diffuse/orchestrator/repeat-shuffle";

  // SIGNALS

  #repeat = signal(false);
  #shuffle = signal(false);

  repeat = this.#repeat.get;
  shuffle = this.#shuffle.get;

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    // Broadcast if needed
    if (this.hasAttribute("group")) {
      this.broadcast(this.nameWithGroup, {});
    }

    // Super
    super.connectedCallback();

    /** @type {import("@components/engine/queue/element.js").CLASS} */
    const queue = query(this, "queue-engine-selector");

    // Assign to self
    this.queue = queue;

    // Signals
    const storagePrefix =
      `${this.constructor.prototype.constructor.NAME}/${this.group}/`;

    this.#repeat.value =
      localStorage.getItem(`${storagePrefix}/repeat`) === "true" ? true : false;
    this.#shuffle.value =
      localStorage.getItem(`${storagePrefix}/shuffle`) === "true"
        ? true
        : false;

    // Wait until defined
    await customElements.whenDefined(queue.localName);

    // Effects
    this.effect(() => {
      const trigger = queue.now();
      const _other_trigger = queue.poolHash();

      this.isLeader().then((isLeader) => {
        if (!isLeader) return;
        // TODO: What happens when shuffle changes here? Need to reset queue probably.
        queue.fill({ amount: 10, shuffled: this.#shuffle.value });
        if (!trigger) queue.shift();
      });
    });

    this.effect(() =>
      localStorage.setItem(
        `${storagePrefix}/repeat`,
        this.#repeat.value ? "true" : "false",
      )
    );

    this.effect(() =>
      localStorage.setItem(
        `${storagePrefix}/shuffle`,
        this.#shuffle.value ? "true" : "false",
      )
    );
  }

  // ACTIONS

  /** @param {boolean} bool */
  setRepeat = (bool) => this.#repeat.value = bool;

  /** @param {boolean} bool */
  setShuffle = (bool) => this.#shuffle.value = bool;
}

export default RepeatShuffleOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = RepeatShuffleOrchestrator;
export const NAME = "do-repeat-shuffle";

customElements.define(NAME, CLASS);
