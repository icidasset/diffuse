if ("serviceWorker" in navigator) {
  const swUrl = new URL(import.meta.resolve("./service-worker.js"));
  swUrl.searchParams.set("cache-name", "diffuse-offline");

  navigator.serviceWorker
    .register(swUrl.href, { type: "module", scope: "./" })
    .catch((error) => {
      console.warn("[do-offline] Failed to register service worker:", error);
    });
}
