//
// Elm loader
// | (• ◡•)| (❍ᴥ❍ʋ)
//
// The bit where we launch the Elm app,
// and connect the other bits to it.


import "tocca"
import JSZip from "jszip"
import { saveAs } from "file-saver"

import "../../build/vendor/pep"

import * as audioEngine from "./audio-engine"
import { debounce, fileExtension } from "./common"


// 🍱


const app = Elm.UI.init({
  node: document.getElementById("elm"),
  flags: {
    darkMode: preferredColorScheme().matches,
    initialTime: Date.now(),
    isOnline: navigator.onLine,
    upgrade: viableForUpgrade(),
    viewport: {
      height: window.innerHeight,
      width: window.innerWidth
    }
  }
})

self.app = app



// Brain
// =====

const brain = new Worker(
  "brain.js#appHref=" +
  encodeURIComponent(window.location.href)
)


app.ports.toBrain.subscribe(thing => {
  brain.postMessage(thing)
})


brain.onmessage = event => {
  if (event.data.action) return handleAction(event.data.action, event.data.data)
  if (event.data.tag) return app.ports.fromAlien.send(event.data)
}


function handleAction(action, data) { switch (action) {
  case "DOWNLOAD_TRACKS": return downloadTracks(data)
  case "REDIRECT_TO_BLOCKSTACK": return redirectToBlockstack(data)
}}



// Audio
// -----

const orchestrion = {
  activeQueueItem: null,
  audio: null,
  app: app,
  repeat: false
}


audioEngine.setup(orchestrion)


app.ports.activeQueueItemChanged.subscribe(item => {
  const timestampInMilliseconds = Date.now()

  orchestrion.activeQueueItem = item
  orchestrion.audio = null

  if (orchestrion.scrobbleTimer) {
    orchestrion.scrobbleTimer.stop()
    orchestrion.scrobbleTimer = null
  }

  audioEngine.usesSingleAudioNode()
    ? false
    : audioEngine.removeOlderAudioElements(timestampInMilliseconds)

  if (item) {
    audioEngine.insertTrack(orchestrion, item)
  } else {
    app.ports.setAudioIsPlaying.send(false)
    app.ports.setAudioPosition.send(0)
  }
})


app.ports.adjustEqualizerSetting.subscribe(e => {
  audioEngine.adjustEqualizerSetting(e.knob, e.value)
})


app.ports.pause.subscribe(_ => {
  if (orchestrion.audio) orchestrion.audio.pause()
})


app.ports.play.subscribe(_ => {
  if (orchestrion.audio) orchestrion.audio.play()
})


app.ports.preloadAudio.subscribe(debounce(item => {
  // Wait 15 seconds to preload something.
  // This is particularly useful when quickly shifting through tracks,
  // or when moving things around in the queue.
  (audioEngine.usesSingleAudioNode() || item.isCached)
    ? false
    : audioEngine.preloadAudioElement(orchestrion, item)
}, 15000))


app.ports.seek.subscribe(percentage => {
  audioEngine.seek(orchestrion.audio, percentage)
})


app.ports.setRepeat.subscribe(repeat => {
  orchestrion.repeat = repeat
})



// Authentication
// --------------

function redirectToBlockstack(authRequest) {
  switch (location.hostname) {
    case "0.0.0.0":
    case "127.0.0.1":
    case "localhost":
      return window.location.href = `http://localhost:8888/auth?authRequest=${authRequest}`

    default:
      return window.location.href = `https://browser.blockstack.org/auth?authRequest=${authRequest}`
  }
}



// Backdrop
// --------

function averageColorOfImage(img) {
  const canvas = document.createElement("canvas")
  const ctx = canvas.getContext("2d")
  canvas.width = img.naturalWidth
  canvas.height = img.naturalHeight

  ctx.drawImage(img, 0, 0)

  const imageData = ctx.getImageData(0, 0, canvas.width, canvas.height)
  const color = { r: 0, g: 0, b: 0 }

  for (let i = 0, l = imageData.data.length; i < l; i += 4) {
    color.r += imageData.data[i]
    color.g += imageData.data[i + 1]
    color.b += imageData.data[i + 2]
  }

  color.r = Math.floor(color.r / (imageData.data.length / 4))
  color.g = Math.floor(color.g / (imageData.data.length / 4))
  color.b = Math.floor(color.b / (imageData.data.length / 4))

  return color
}


app.ports.pickAverageBackgroundColor.subscribe(src => {
  const img = document.querySelector(`img[src$="${src}"]`)

  if (img) {
    const avgColor = averageColorOfImage(img)
    app.ports.setAverageBackgroundColor.send(avgColor)
  }
})



// Clipboard
// ---------

app.ports.copyToClipboard.subscribe(text => {

  // Insert a textarea element
  const el = document.createElement("textarea")

  el.value = text
  el.setAttribute("readonly", "")
  el.style.position = "absolute"
  el.style.left = "-9999px"

  document.body.appendChild(el)

  // Store original selection
  const selected = document.getSelection().rangeCount > 0
    ? document.getSelection().getRangeAt(0)
    : false

  // Select & copy the text
  el.select()
  document.execCommand("copy")

  // Remove textarea element
  document.body.removeChild(el)

  // Restore original selection
  if (selected) {
    document.getSelection().removeAllRanges()
    document.getSelection().addRange(selected)
  }

})



// Dark mode
// ---------

function preferredColorScheme() {
  const m =
    window.matchMedia &&
    window.matchMedia("(prefers-color-scheme: dark)")

  m && m.addEventListener && m.addEventListener("change", e => {
    app.ports.preferredColorSchemaChanged.send({ dark: e.matches })
  })

  return m
}



// Downloading
// -----------

function downloadTracks(group) {
  const zip = new JSZip()
  const folder = zip.folder("Diffuse - " + group.name)

  return group.tracks.reduce(
    (acc, track) => { return acc
      .then(_ => fetch(track.url))
      .then(r => {
        const mimeType = r.headers.get("content-type")
        const fileExt = fileExtension(mimeType) || "unknown"

        return r.blob().then(
          b => folder.file(track.filename + "." + fileExt, b)
        )
      })
    },
    Promise.resolve()

  ).then(_ => zip.generateAsync({ type: "blob" })
  ).then(zipFile => {
    saveAs(zipFile, "Diffuse - " + group.name + ".zip")
    app.ports.downloadTracksFinished.send(null)

  })
}



// Focus
// -----

document.body.addEventListener("click", event => {
  if (
    event.target.matches("button, a") ||
    event.target.closest("button, a")
  ) {
    removeFocus()
  }
})


function removeFocus() {
  const n = document.activeElement
  if (n && !n.dataset.keepFocus) n.blur()
}



// Forms
// -----
// Adds a `changed` attribute to form fields, if the form was "changed".
// This is to help with styling, we don't want to show an error immediately.

const FIELD_SELECTOR = "input, textarea"


document.addEventListener("keyup", e => {
  const field = e.target.closest(FIELD_SELECTOR)
  if (field) field.setAttribute("changed", "")
})


document.addEventListener("click", e => {
  if (e.target.tagName !== "BUTTON") return;
  const form = e.target.closest("form")
  if (form) markAllFormFieldsAsChanged(form)
})


document.addEventListener("submit", e => {
  const form = e.target.closest("form")
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
  app.ports.setIsOnline.send(navigator.onLine)
}



// Media Keys
// ----------
// https://github.com/borismus/keysocket#api
// https://developers.google.com/web/updates/2019/02/chrome-73-media-updates

document.addEventListener("MediaPlayPause", () => {
  app.ports.requestPlayPause.send(null)
})


document.addEventListener("MediaStop", () => {
  app.ports.requestStop.send(null)
})


document.addEventListener("MediaPrev", () => {
  app.ports.requestPrevious.send(null)
})


document.addEventListener("MediaNext", () => {
  app.ports.requestNext.send(null)
})



if ("mediaSession" in navigator) {

  navigator.mediaSession.setActionHandler("play", () => {
    app.ports.requestPlayPause.send(null)
  })


  navigator.mediaSession.setActionHandler("pause", () => {
    app.ports.requestPlayPause.send(null)
  })


  navigator.mediaSession.setActionHandler("previoustrack", () => {
    app.ports.requestPrevious.send(null)
  })


  navigator.mediaSession.setActionHandler("nexttrack", () => {
    app.ports.requestNext.send(null)
  })


  navigator.mediaSession.setActionHandler("seekbackward", _ => {
    const audio = orchestrion.audio
    if (audio) audio.currentTime = Math.max(audio.currentTime - 10, 0)
  })


  navigator.mediaSession.setActionHandler("seekforward", _ => {
    const audio = orchestrion.audio
    if (audio) audio.currentTime = Math.min(audio.currentTime + 10, audio.duration)
  })


  // navigator.mediaSession.setActionHandler("seekto", event => {
  //   const audio = orchestrion.audio
  //   if (audio) audio.currentTime = event.seekTime
  // })

}



// Pointer Events
// --------------
// Thanks to https://github.com/mpizenberg/elm-pep/

let enteredElement


tocca({
  dbltapThreshold: 400,
  tapThreshold: 250
})


function mousePointerEvent(eventType, mouseEvent) {
  let pointerEvent = new MouseEvent(eventType, mouseEvent)
  pointerEvent.pointerId = 1
  pointerEvent.isPrimary = true
  pointerEvent.pointerType = "mouse"
  pointerEvent.width = 1
  pointerEvent.height = 1
  pointerEvent.tiltX = 0
  pointerEvent.tiltY = 0

  "buttons" in mouseEvent && mouseEvent.buttons !== 0
    ? (pointerEvent.pressure = 0.5)
    : (pointerEvent.pressure = 0)

  return pointerEvent
}


function touchPointerEvent(eventType, touchEvent, touch) {
  let pointerEvent = new CustomEvent(eventType, {
    bubbles: true,
    cancelable: true
  })

  pointerEvent.ctrlKey = touchEvent.ctrlKey
  pointerEvent.shiftKey = touchEvent.shiftKey
  pointerEvent.altKey = touchEvent.altKey
  pointerEvent.metaKey = touchEvent.metaKey

  pointerEvent.clientX = touch.clientX
  pointerEvent.clientY = touch.clientY
  pointerEvent.screenX = touch.screenX
  pointerEvent.screenY = touch.screenY
  pointerEvent.pageX = touch.pageX
  pointerEvent.pageY = touch.pageY

  const rect = touch.target.getBoundingClientRect()
  pointerEvent.offsetX = touch.clientX - rect.left
  pointerEvent.offsetY = touch.clientY - rect.top
  pointerEvent.pointerId = 1 + touch.identifier

  pointerEvent.button = 0
  pointerEvent.buttons = 1
  pointerEvent.movementX = 0
  pointerEvent.movementY = 0
  pointerEvent.region = null
  pointerEvent.relatedTarget = null
  pointerEvent.x = pointerEvent.clientX
  pointerEvent.y = pointerEvent.clientY

  pointerEvent.pointerType = "touch"
  pointerEvent.width = 1
  pointerEvent.height = 1
  pointerEvent.tiltX = 0
  pointerEvent.tiltY = 0
  pointerEvent.pressure = 1
  pointerEvent.isPrimary = true

  return pointerEvent
}


// Simulate `pointerenter` and `pointerleave` event for non-touch devices
if (!self.PointerEvent) {
  document.addEventListener("mouseover", event => {
    const section     = document.body.querySelector("section")
    const isDragging  = section && section.classList.contains("dragging-something")
    const node        = isDragging && document.elementFromPoint(event.clientX, event.clientY)

    if (node && node != enteredElement) {
      enteredElement && enteredElement.dispatchEvent(mousePointerEvent("pointerleave", event))
      node.dispatchEvent(mousePointerEvent("pointerenter", event))
      enteredElement = node
    }
  })
}


// Simulate `pointerenter` and `pointerleave` event for touch devices
document.body.addEventListener("touchmove", event => {
  const section       = document.body.querySelector("section")
  const isDragging    = section && section.classList.contains("dragging-something")

  let touch = event.touches[0]
  let node

  if (isDragging && touch) {
    node = document.elementFromPoint(touch.clientX, touch.clientY)
  }

  if (node && node != enteredElement) {
    enteredElement && enteredElement.dispatchEvent(touchPointerEvent("pointerleave", event, touch))
    node.dispatchEvent(touchPointerEvent("pointerenter", event, touch))
    enteredElement = node
  }

  if (isDragging) {
    event.stopPropagation()
  }
})



// Touch Device
// ------------

window.addEventListener("touchstart", function onFirstTouch() {
  app.ports.indicateTouchDevice.send(null)
  window.removeEventListener("touchstart", onFirstTouch, false)
}, false)



// Upgrade
// -------

function viableForUpgrade() {
  // Was the user using an old version of the app?
  // V1
  const viable_v1 = !!localStorage.getItem("additional-userdata")
  if (viable_v1) localStorage.removeItem("additional-userdata")

  // The end
  return viable_v1
}




// Vertical Height
// ---------------

setVerticalHeightUnit()


window.addEventListener("resize", () => {
  setTimeout(setVerticalHeightUnit, 0)
})


function setVerticalHeightUnit() {
  const vh = document.documentElement.clientHeight * 0.01
  document.documentElement.style.setProperty("--vh", `${vh}px`)
}
