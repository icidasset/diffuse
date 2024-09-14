import type { App } from "./elm/types"


// 🏔️


let app: App



// 🚀


export function init(a: App) {
  app = a

  app.ports.copyToClipboard.subscribe(copyToClipboard)
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
