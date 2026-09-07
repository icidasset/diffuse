if ("serviceWorker" in navigator) {
  const swUrl = new URL(import.meta.resolve("./service-worker.js"));
  swUrl.searchParams.set("cache-name", "diffuse-offline");

  // Some deployments serve the page under a version alias (e.g. `/4.x-nightly/`)
  // but redirect the service worker to its content-addressed CID path
  // (`/bafy…/`). Scoping to `./` resolves against the page URL, which then sits
  // outside the script's serving directory and registration is rejected.
  // Resolve the script to its final (post-redirect) URL first and scope to
  // *that* directory, so no `Service-Worker-Allowed` header is required.
  // A HEAD request is enough to learn the redirect target without transferring
  // the script body (which embeds the file tree).
  fetch(swUrl.href, { method: "HEAD", cache: "no-store" })
    .then((response) => response.url || swUrl.href)
    .then((scriptUrl) =>
      navigator.serviceWorker.register(scriptUrl, {
        type: "module",
        scope: new URL("./", scriptUrl).href,
      }))
    .catch((error) => {
      console.warn("[do-offline] Failed to register service worker:", error);
    });

  // When a new SW takes over, the page needs to reload so it runs fresh code
  // under the new controller. Two signals can fire when this happens, and
  // browsers differ in which they deliver (and in what order):
  //   - the SW explicitly posts "sw-activated" after clients.claim(), and
  //   - the page fires "controllerchange" when claim() takes effect.
  // Firefox in particular has been observed to occasionally drop the former
  // while still switching controllers, leaving the page running stale code
  // until the next manual refresh. Listen for both and funnel them through a
  // single debounced reload.
  //
  // The debounce breaks the reload loop that can occur when the SW script
  // itself changes between reloads (esbuild chunk-hash churn in dev): we store
  // a timestamp, and any second trigger within that window is the loop and is
  // skipped. A stale timestamp means a genuinely new SW arrived later, so we
  // reload again. The timestamp lives in sessionStorage so the guard survives
  // the navigation.
  const RELOAD_GUARD_MS = 5000;

  function reloadForNewController() {
    const flag = sessionStorage.getItem("sw-activated-reload");
    if (flag) {
      sessionStorage.removeItem("sw-activated-reload");
      if (Date.now() - Number(flag) < RELOAD_GUARD_MS) return;
    }
    sessionStorage.setItem("sw-activated-reload", String(Date.now()));
    location.reload();
  }

  navigator.serviceWorker.addEventListener("message", (event) => {
    if (event.data?.type !== "sw-activated") return;
    reloadForNewController();
  });

  navigator.serviceWorker.addEventListener("controllerchange", () => {
    // "controllerchange" also fires if the page loses its controller entirely
    // (e.g. the SW is unregistered) — don't reload in that case, only when a
    // (new) controller actually took control.
    if (navigator.serviceWorker.controller) reloadForNewController();
  });
}
