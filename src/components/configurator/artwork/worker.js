import { ostiary, rpc, workerProxy } from "~/common/worker.js";

/**
 * @import {ActionsWithTunnel, ProxiedActions} from "~/common/worker.d.ts"
 * @import {Actions} from "@specs/components/artwork/types.d.ts"
 * @import {Actions as ConfiguratorActions} from "@specs/components/configurator/artwork/types.d.ts"
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * Time budget for a single artwork retrieval component (provider). A provider
 * that takes longer than this is skipped and the chain falls through to the
 * next one, so one slow source can't hold up the whole lookup.
 */
const PROVIDER_TIMEOUT_MS = 60_000;

/**
 * When a provider fails on a long-running request (times out, or rejects), we
 * "open the circuit" for it for this long: subsequent requests skip that
 * provider entirely and fall straight through to the next one, instead of
 * repeatedly hammering a source that's currently failing.
 */
const PROVIDER_COOLDOWN_MS = 5 * 60_000;

/**
 * A short sentinel so the provider result is distinguishable from a real
 * `null` "no art" reply.
 */
const TIMED_OUT = "__PROVIDER_TIMEOUT__";

/**
 * providerKey → timestamp (ms) until which that provider is cooled down.
 */
/** @type {Map<string, number>} */
const cooldownUntil = new Map();

/**
 * @type {ActionsWithTunnel<ConfiguratorActions>['get']}
 */
export async function get({ data, ports }) {
  const track = data;

  const now = Date.now();
  for (const [key, port] of Object.entries(ports)) {
    // Circuit breaker: if this provider recently failed a long-running request,
    // skip it entirely (no invocation at all) and try the next source. Expired
    // entries simply fall through and get invoked again (and re-cooled if they
    // fail again).
    const until = cooldownUntil.get(key);
    if (until !== undefined && until > now) continue;

    /** @type {ProxiedActions<Actions>} */
    const artwork = workerProxy(() => {
      port.start();
      return port;
    });

    // Race each provider against its own time budget instead of wrapping the
    // whole chain. If it doesn't answer in time, treat it like a failure and
    // fall through to the next configured source.
    const deadline = new Promise((resolve) =>
      setTimeout(() => resolve(TIMED_OUT), PROVIDER_TIMEOUT_MS)
    );

    let bytes;
    let failed = false;
    try {
      bytes = await Promise.race([artwork.get(track), deadline]);
      if (bytes === TIMED_OUT) failed = true;
    } catch {
      bytes = TIMED_OUT;
      failed = true;
    }

    if (failed) {
      // A long-running request failed — open the circuit for a while.
      cooldownUntil.set(key, Date.now() + PROVIDER_COOLDOWN_MS);
      continue;
    }
    if (bytes !== null) return bytes;
  }

  return null;
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, { get });
});
