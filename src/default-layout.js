if ("serviceWorker" in navigator) {
  const swUrl = new URL(import.meta.resolve("./service-worker.js"));
  swUrl.searchParams.set("cache-name", "diffuse-offline");

  navigator.serviceWorker
    .register(swUrl.href, { type: "module", scope: "./" })
    .catch((error) => {
      console.warn("[do-offline] Failed to register service worker:", error);
    });

  // When the SW activates it sends "sw-activated". Reload so the page runs
  // fresh code under the new SW. The sessionStorage flag skips one reload on
  // the very next page load to break the loop caused by the SW script URL
  // changing during the reload (e.g. esbuild chunk-hash churn in development).
  // The listener is always attached so a second activation in the same load
  // isn't silently dropped.
  navigator.serviceWorker.addEventListener("message", (event) => {
    if (event.data?.type !== "sw-activated") return;
    if (sessionStorage.getItem("sw-activated-reload")) {
      sessionStorage.removeItem("sw-activated-reload");
      return;
    }
    sessionStorage.setItem("sw-activated-reload", "1");
    location.reload();
  });
}
