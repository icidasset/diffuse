import { DiffuseElement } from "~/common/element.js";

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Registers a service worker that makes the page available offline.
 *
 * All resources fetched by the page are cached as they load.
 * While online, requests always go to the network (the cache is bypassed),
 * and successful responses are stored for later. While offline, the cache
 * is used as a fallback.
 *
 * Attributes:
 *   cache-name      Name of the cache to use (default: "diffuse-offline")
 *   scope           Service worker scope (default: "./")
 *   src             URL of the service worker script (default: built-in service-worker.js)
 *                   Must be served from a path within the requested scope, or the server
 *                   must include a `Service-Worker-Allowed: /` response header.
 */
class OfflineOrchestrator extends DiffuseElement {
  static NAME = "diffuse/orchestrator/offline";

  // LIFECYCLE

  /** @override */
  async connectedCallback() {
    super.connectedCallback();

    if (!("serviceWorker" in navigator)) return;

    const cacheName = this.getAttribute("cache-name") ?? "diffuse-offline";
    const scope = this.getAttribute("scope") ?? "./";
    const src = this.getAttribute("src");

    const swUrl = new URL(
      src ?? import.meta.resolve("../../../service-worker-offline.js"),
    );

    swUrl.searchParams.set("cache-name", cacheName);

    try {
      await navigator.serviceWorker.register(swUrl.href, {
        type: "module",
        scope,
      });
    } catch (error) {
      console.warn("[do-offline] Failed to register service worker:", error);
    }
  }
}

export default OfflineOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = OfflineOrchestrator;
export const NAME = "do-offline";

customElements.define(NAME, OfflineOrchestrator);
