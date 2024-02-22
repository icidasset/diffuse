import "./index.d"
import { version } from "../../../package.json"


export const load = ({ isNativeWrapper, reg }: { isNativeWrapper: boolean, reg: ServiceWorkerRegistration }) => Elm.UI.init({
  node: document.getElementById("elm") || undefined,
  flags: {
    buildTimestamp: BUILD_TIMESTAMP,
    darkMode: preferredColorScheme().matches,
    initialTime: Date.now(),
    isInstallingServiceWorker: !!reg.installing,
    isOnline: navigator.onLine,
    isTauri: isNativeWrapper,
    version,
    viewport: {
      height: window.innerHeight,
      width: window.innerWidth
    }
  }
})



// 🌗


function preferredColorScheme() {
  const m =
    window.matchMedia &&
    window.matchMedia("(prefers-color-scheme: dark)")

  m?.addEventListener("change", e => {
    app.ports.preferredColorSchemaChanged.send({ dark: e.matches })
  })

  return m
}
