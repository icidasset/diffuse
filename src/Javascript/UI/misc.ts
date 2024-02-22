import { App } from "./elm/types"


// 🏔️


let app: App
let channel: BroadcastChannel



// 🚀


export function init(a: App, c: BroadcastChannel) {
  app = a
  channel = c

  app.ports.copyToClipboard.subscribe(copyToClipboard)
  app.ports.downloadJsonUsingTauri.subscribe(downloadJsonUsingTauri)
  app.ports.openUrlOnNewPage.subscribe(openUrlOnNewPage)
  app.ports.reloadApp.subscribe(reloadApp)
}



// Application
// -----------

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
  let timeout = setTimeout(async () => {
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



// Clipboard
// ---------


async function copyToClipboard(text: string) {
  navigator.clipboard.writeText(text)
}



// Focus
// -----

window.addEventListener("blur", event => {
  if (app && event.target === window) app.ports.lostWindowFocus.send(null)
})



// Forms
// -----
// Adds a `changed` attribute to form fields, if the form was "changed".
// This is to help with styling, we don't want to show an error immediately.

const FIELD_SELECTOR = "input, textarea"


document.addEventListener("keyup", e => {
  const field = e.target && (<HTMLElement>e.target).closest(FIELD_SELECTOR)
  if (field) field.setAttribute("changed", "")
})


document.addEventListener("click", e => {
  if (!e.target || (<HTMLElement>e.target).tagName !== "BUTTON") return;
  const form = (<HTMLElement>e.target).closest("form")
  if (form) markAllFormFieldsAsChanged(form)
})


document.addEventListener("submit", e => {
  const form = e.target && (<HTMLElement>e.target).closest("form")
  if (form) markAllFormFieldsAsChanged(form)
})


function markAllFormFieldsAsChanged(form) {
  [].slice.call(form.querySelectorAll(FIELD_SELECTOR)).forEach(field => {
    field.setAttribute("changed", "")
  })
}



// Internet Connection
// -------------------

window.addEventListener("online", onlineStatusChanged)
window.addEventListener("offline", onlineStatusChanged)


function onlineStatusChanged() {
  if (app) app.ports.setIsOnline.send(navigator.onLine)
}



// Touch Device
// ------------

window.addEventListener("touchstart", function onFirstTouch() {
  if (!app) return
  app.ports.indicateTouchDevice.send()
  window.removeEventListener("touchstart", onFirstTouch, false)
}, false)
