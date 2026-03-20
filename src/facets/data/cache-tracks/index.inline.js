import { BroadcastableDiffuseElement } from "~/common/element.js";
import { effect, signal } from "~/common/signal.js";
import foundation from "~/common/foundation.js";


/**
 * @import {ScrobbleElement} from "~/components/supplement/types.d.ts"
 * @import {Track} from "~/definitions/types.d.ts"
 */

let initialised = false;

effect(() => {
  const audio = foundation.signals.engine.audio();
  if (!audio) return;

  if (!initialised) {
    setupScrobbler();
    initialised = true;
  }
});

/**
 * Pretend we're a scrobbler and cache tracks whenever they are "scrobbled"
 */
async function setupScrobbler() {
  await foundation.orchestrator.scrobbleAudio();
  await customElements.whenDefined("dct-scrobbler");

  const s = new PretendScrobbler();
  s.setAttribute("group", foundation.GROUP);

  const c = await foundation.configurator.scrobbles();
  c.append(s);
}

/**
 * @implements {ScrobbleElement}
 */
class PretendScrobbler extends BroadcastableDiffuseElement {
  isAuthenticated = signal(true).get;
  isAuthenticating = signal(false).get;
  handle = signal(null).get;

  async nowPlaying() {}

  /**
   * @param {Track} track
   */
  async scrobble(track) {
    const i = await foundation.configurator.input();
    await i.cache([track.uri]);
  }

  // LIFECYCLE

  /** @override */
  connectedCallback() {
    // Broadcast if needed
    if (this.hasAttribute("group")) {
      const actions = this.broadcast(this.identifier, {
        nowPlaying: { strategy: "leaderOnly", fn: this.nowPlaying },
        scrobble: { strategy: "leaderOnly", fn: this.scrobble },
      });

      if (actions) {
        this.nowPlaying = actions.nowPlaying;
        this.scrobble = actions.scrobble;
      }
    }

    super.connectedCallback();
  }
}

customElements.define("dct-scrobbler", PretendScrobbler);
