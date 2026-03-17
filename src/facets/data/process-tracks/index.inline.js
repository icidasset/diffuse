import foundation from "~/common/foundation.js";

const KEY = "facets/data/process-tracks/timestamp";
const lastTimestamp = localStorage.getItem(KEY);
const now = Date.now();

if (lastTimestamp) {
  const diff = now - JSON.parse(lastTimestamp);

  if (diff >= 10 * 60 * 1000) {
    foundation.orchestrator.processTracks({
      disableWhenReady: false,
    });

    localStorage.setItem(KEY, JSON.stringify(now));
  }
} else {
  foundation.orchestrator.processTracks({
    disableWhenReady: false,
  });

  localStorage.setItem(KEY, JSON.stringify(now));
}
