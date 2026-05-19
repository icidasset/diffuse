if ("serviceWorker" in navigator) {
  const swUrl = new URL(import.meta.resolve("./service-worker.js"));
  swUrl.searchParams.set("cache-name", "diffuse-offline");

  navigator.serviceWorker
    .register(swUrl.href, { type: "module", scope: "./" })
    .catch((error) => {
      console.warn("[do-offline] Failed to register service worker:", error);
    });

  // When the SW activates it sends "sw-activated". Reload so the page runs
  // fresh code under the new SW. To break the loop that can occur when the SW
  // script itself changes between reloads (esbuild chunk-hash churn in dev),
  // we store a timestamp. If a second sw-activated arrives within a few seconds
  // of the previous reload it is the loop — skip it. A stale timestamp means a
  // genuinely new SW arrived later, so we reload again.
  const RELOAD_GUARD_MS = 5000;
  navigator.serviceWorker.addEventListener("message", (event) => {
    if (event.data?.type !== "sw-activated") return;
    const flag = sessionStorage.getItem("sw-activated-reload");
    if (flag) {
      sessionStorage.removeItem("sw-activated-reload");
      if (Date.now() - Number(flag) < RELOAD_GUARD_MS) return;
    }
    sessionStorage.setItem("sw-activated-reload", String(Date.now()));
    location.reload();
  });
}
