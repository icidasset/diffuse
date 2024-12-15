import type { App } from "./elm/types"


/**
 * Load:
 *
 * 1. Redirect to HTTPS if using the `diffuse.sh` domain (subdomains included).
 * 2. Fail if not a secure context.
 * 3. Set up service worker, ensure it's ready and then continue initialisation.
 */
export async function load({ isNativeWrapper } : { isNativeWrapper: boolean }): Promise<ServiceWorkerRegistration> {
  return new Promise((resolve, reject) => {
    if (location.hostname.endsWith("diffuse.sh") && location.protocol === "http:") {
      location.href = location.href.replace("http://", "https://")
      reject("Just a moment, redirecting to HTTPS.")

    } else if (!self.isSecureContext) {
      reject(`
        This app only works on a <a class="underline" target="_blank" href="https://developer.mozilla.org/en-US/docs/Web/Security/Secure_Contexts#When_is_a_context_considered_secure">secure context</a>, HTTPS & localhost, and modern browsers.
      `)

    } else if ("serviceWorker" in navigator) {
      // Service worker
      window.addEventListener("load", () => {
        navigator.serviceWorker
          .getRegistrations()
          .then(async registrations => {
            const serverIsOnline = navigator.onLine && await fetch(`${location.origin}?ping=1`)
              .then(r => r.text())
              .then(a => a === "false" ? false : true)

            if (isNativeWrapper) await Promise.all(
              registrations.map(r => r.unregister())
            )

            if (serverIsOnline) return navigator.serviceWorker.register(
              "service-worker.js",
              // { type: "module" }
            )

            if (registrations[0]) return registrations

            throw new Error("Web server is offline")
          })
          .then(() => navigator.serviceWorker.ready)
          .then(resolve)
          .catch(err => {
            const isFirefox = navigator.userAgent.toLowerCase().includes("firefox")

            console.error(err)
            return reject(
              location.protocol === "https:" || location.hostname === "localhost"
                ? "Failed to start the service worker." + (isFirefox ? " Make sure the setting <strong>Delete cookies and site data when Firefox is closed</strong> is off, or Diffuse's domain is added as an exception." : "")
                : "Failed to start the service worker, try using HTTPS."
            )
          })
      })

    }
  })
}


/**
 * Link.
 */
export function link(
  { app, isNativeWrapper, reg } : { app: App, isNativeWrapper: boolean, reg: ServiceWorkerRegistration }
) {
  if (reg.installing) console.log("🧑‍✈️ Service worker is installing")
  const initialInstall = reg.installing

  initialInstall?.addEventListener("statechange", function() {
    if (this.state === "activated") {
      console.log("🧑‍✈️ Service worker is activated")
      app.ports.installedNewServiceWorker.send(null)
    }
  })

  if (reg.waiting) {
    console.log("🧑‍✈️ A new version of Diffuse is available")
    app.ports.installingNewServiceWorker.send(null)
    app.ports.installedNewServiceWorker.send(null)
  }

  if (initialInstall?.state === "activated") {
    console.log("🧑‍✈️ Service worker is activated")
    app.ports.installedNewServiceWorker.send(null)
  }

  reg.addEventListener("updatefound", () => {
    const newWorker = reg.installing
    if (!newWorker) return

    // No worker was installed yet, so we'll only want to track the state changes
    if (newWorker !== initialInstall) {
      console.log("🧑‍✈️ A new version of Diffuse is available")
      app.ports.installingNewServiceWorker.send(null)
    }

    newWorker.addEventListener("statechange", (e: any) => {
      console.log("🧑‍✈️ Service worker is", e.target.state)
      if (e.target.state === "installed") app.ports.installedNewServiceWorker.send(null)
    })
  })

  // Check for service worker updates and every hour after that
  if (!isNativeWrapper && navigator.onLine) {
    reg.update()
    setInterval(() => reg.update(), 1 * 1000 * 60 * 60)
  }
}
