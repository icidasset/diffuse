import "./index.d"
import type { App } from "./elm/types"
import { version } from "../../../package.json"


// 🏔️


let app: App
let channel: BroadcastChannel



// 🚀


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


export function init(a: App, c: BroadcastChannel) {
  app = a
  channel = c

  app.ports.downloadJsonUsingTauri.subscribe(downloadJsonUsingTauri)
  app.ports.openUrlOnNewPage.subscribe(openUrlOnNewPage)
  app.ports.reloadApp.subscribe(reloadApp)
}



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



// PORTS


async function downloadJsonUsingTauri(
  { filename, json }: { filename: string, json: string }
) {
  const { save } = await import("@tauri-apps/plugin-dialog")
  const { writeTextFile } = await import("@tauri-apps/plugin-fs")
  const { BaseDirectory } = await import("@tauri-apps/api/path")

  const filePath = await save({ defaultPath: filename })
  await writeTextFile(filePath || filename, json, { baseDir: BaseDirectory.Download })
}


function openUrlOnNewPage(url: string) {
  if (globalThis.__TAURI__) {
    globalThis.__TAURI__.shell.open(
      url.includes("://") ? url : `${location.origin}/${url.replace(/^\.\//, "")}`
    )

  } else {
    window.open(url, "_blank")

  }
}


function reloadApp() {
  const timeout = setTimeout(async () => {
    const reg = await navigator.serviceWorker.getRegistration()
    if (reg?.waiting) reg.waiting.postMessage("skipWaiting")
    window.location.reload()
  }, 250)

  channel.addEventListener("message", event => {
    if (event.data === "PONG") {
      clearTimeout(timeout)
      alert("⚠️ You can only update the app when you have no more than one instance open.")
    }
  })

  channel.postMessage("PING")
}
