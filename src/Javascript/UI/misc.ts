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
