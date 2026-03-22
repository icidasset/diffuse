import foundation from "~/common/foundation.js";

const KEY = "facets/data/process-tracks/timestamp";
const MAX_TIME_DIFF = 10 * 60 * 1000;

const lastTimestamp = localStorage.getItem(KEY);
const now = Date.now();
const diff = lastTimestamp ? now - JSON.parse(lastTimestamp) : MAX_TIME_DIFF;

if (diff >= MAX_TIME_DIFF) {
  const orchestrator = await foundation.orchestrator.processTracks({
    disableWhenReady: true,
  });

  // Wait until we're actually done processing, only then set the timestamp
  await orchestrator.process();
  localStorage.setItem(KEY, JSON.stringify(now));
}
