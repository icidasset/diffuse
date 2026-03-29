import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

effect(() => {
  // Trigger setup when audio is used
  if (foundation.signals.engine.audio()) {
    setup();
  }
});

async function setup() {
  const [audio, input, output, queue] = await Promise.all([
    foundation.engine.audio(),
    foundation.configurator.input(),
    foundation.orchestrator.output(),
    foundation.engine.queue(),
  ]);

  await Promise.all([
    customElements.whenDefined(audio.localName),
    customElements.whenDefined(input.localName),
    customElements.whenDefined(queue.localName),
  ]);

  /** @type {number | undefined} */
  let preloadTimeout;

  effect(() => {
    const nextItem = queue.future()[0] ?? null;
    const tracksCol = output?.tracks.collection();
    const tracks = tracksCol?.state === "loaded" ? tracksCol.data : undefined;
    const nextTrack = nextItem
      ? tracks?.find((t) => t.id === nextItem.id)
      : undefined;

    clearTimeout(preloadTimeout);
    if (!nextTrack) return;

    preloadTimeout = setTimeout(async () => {
      const resolvedNextUri = await input.resolve({
        method: "GET",
        uri: nextTrack.uri,
      });

      const nextUrl = resolvedNextUri && !("stream" in resolvedNextUri)
        ? resolvedNextUri.url
        : undefined;

      if (!nextUrl) return;

      const activeItems = audio.items().filter((i) => !i.isPreload);

      audio.supply({
        audio: [...activeItems, {
          id: nextItem.id,
          isPreload: true,
          url: nextUrl,
          track: nextTrack,
        }],
      });
    }, 30_000);
  });
}
