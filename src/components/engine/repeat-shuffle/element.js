import { BroadcastableDiffuseElement } from "@common/element.js";
import { signal } from "@common/signal.js";

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class RepeatShuffleEngine extends BroadcastableDiffuseElement {
  static NAME = "diffuse/engine/repeat-shuffle";

  // SIGNALS

  #repeat = signal(false);
  #shuffle = signal(false);

  repeat = this.#repeat.get;
  shuffle = this.#shuffle.get;

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    // Broadcast if needed
    if (this.hasAttribute("group")) {
      const actions = this.broadcast(this.nameWithGroup, {
        setRepeat: { strategy: "replicate", fn: this.setRepeat },
        setShuffle: { strategy: "replicate", fn: this.setShuffle },
      });

      if (actions) {
        this.setRepeat = actions.setRepeat;
        this.setShuffle = actions.setShuffle;
      }
    }

    // Super
    super.connectedCallback();

    // Signals
    const storagePrefix =
      `${this.constructor.prototype.constructor.NAME}/${this.group}/`;

    this.#repeat.value =
      localStorage.getItem(`${storagePrefix}/repeat`) === "true" ? true : false;
    this.#shuffle.value =
      localStorage.getItem(`${storagePrefix}/shuffle`) === "true"
        ? true
        : false;

    // Effects
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
  setRepeat = async (bool) => this.#repeat.value = bool;

  /** @param {boolean} bool */
  setShuffle = async (bool) => this.#shuffle.value = bool;
}

export default RepeatShuffleEngine;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = RepeatShuffleEngine;
export const NAME = "de-repeat-shuffle";

customElements.define(NAME, CLASS);
