import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

effect(() => {
  // Trigger setup when audio is used
  if (foundation.signals.engine.audio()) {
    setup();
  }
});

async function setup() {
  await foundation.orchestrator.scrobbleAudio();
  const configurator = await foundation.configurator.scrobbles();

  // Bundled scrobblers
  const { default: LastFmScrobbler } = await import(
    "~/components/supplement/last.fm/element.js"
  );

  const lastFm = new LastFmScrobbler();
  lastFm.setAttribute("group", foundation.GROUP);
  configurator.append(lastFm);

  const { default: RockskyScrobbler } = await import(
    "~/components/supplement/rocksky/element.js"
  );

  const rocksky = new RockskyScrobbler();
  rocksky.setAttribute("group", foundation.GROUP);
  configurator.append(rocksky);
}
