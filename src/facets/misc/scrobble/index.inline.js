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
  const [configurator, output] = await Promise.all([
    foundation.configurator.scrobbles(),
    foundation.orchestrator.output(),
  ]);

  // Bundled scrobblers
  const { default: LastFmScrobbler } = await import(
    "~/components/supplement/last.fm/element.js"
  );

  const lastFm = new LastFmScrobbler();
  lastFm.setAttribute("group", foundation.GROUP);
  lastFm.setAttribute("output-selector", output.selector);
  configurator.append(lastFm);

  const { default: RockskyScrobbler } = await import(
    "~/components/supplement/rocksky/element.js"
  );

  const rocksky = new RockskyScrobbler();
  rocksky.setAttribute("group", foundation.GROUP);
  rocksky.setAttribute("output-selector", output.selector);
  configurator.append(rocksky);

  const { default: ListenBrainzScrobbler } = await import(
    "~/components/supplement/listenbrainz/element.js"
  );

  const listenBrainz = new ListenBrainzScrobbler();
  listenBrainz.setAttribute("group", foundation.GROUP);
  listenBrainz.setAttribute("output-selector", output.selector);
  configurator.append(listenBrainz);
}
