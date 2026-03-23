import { debounce } from "throttle-debounce";

import * as Output from "~/common/output.js";
import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

const KEY = "facets/data/process-tracks/timestamp";
const MAX_TIME_DIFF = 10 * 60 * 1000;

////////////////////////////////////////////
// ON LOAD
////////////////////////////////////////////

const output = await foundation.orchestrator.output();
await Output.data(output.tracks);

const lastTimestamp = localStorage.getItem(KEY);
const now = Date.now();
const diff = lastTimestamp ? now - JSON.parse(lastTimestamp) : MAX_TIME_DIFF;

const orchestrator = await foundation.orchestrator.processTracks({
  disableWhenReady: true,
});

if (diff >= MAX_TIME_DIFF && await orchestrator.isLeader()) {
  // Wait until we're actually done processing, only then set the timestamp
  await orchestrator.process();
  localStorage.setItem(KEY, JSON.stringify(now));
}

////////////////////////////////////////////
// WHEN SOURCES CHANGE
////////////////////////////////////////////

const { sources } = await foundation.orchestrator.sources();
let initialised = false;

const debounced = debounce(
  2500,
  /** @param {import("~/components/input/types.d.ts").Source} _sources */ async (
    _sources,
  ) => {
    if (initialised && await orchestrator.isLeader()) orchestrator.process();
    else initialised = true;
  },
  { atBegin: false },
);

effect(() => {
  debounced(sources());
});
