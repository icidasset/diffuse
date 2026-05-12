if ("serviceWorker" in navigator) {
  const swUrl = new URL(import.meta.resolve("./service-worker.js"));
  swUrl.searchParams.set("cache-name", "diffuse-offline");

  navigator.serviceWorker
    .register(swUrl.href, { type: "module", scope: "./" })
    .catch((error) => {
      console.warn("[do-offline] Failed to register service worker:", error);
    });

  // When the SW activates it sends "sw-activated". Reload once so the page
  // runs fresh code under the new SW. The sessionStorage flag prevents a
  // second reload on the very next page load (which would happen if the SW
  // script changed again during the reload, e.g. esbuild chunk-hash churn).
  if (sessionStorage.getItem("sw-activated-reload")) {
    sessionStorage.removeItem("sw-activated-reload");
  } else {
    navigator.serviceWorker.addEventListener("message", (event) => {
      if (event.data?.type !== "sw-activated") return;
      sessionStorage.setItem("sw-activated-reload", "1");
      location.reload();
    });
  }
}
