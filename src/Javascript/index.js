//
// Elm loader
// | (• ◡•)| (❍ᴥ❍ʋ)
//
// The bit where we launch the Elm app,
// and connect the other bits to it.


import "tocca"
import loadScript from "load-script2"
import JSZip from "jszip"
import { saveAs } from "file-saver"

import "../../build/vendor/pep"

import * as audioEngine from "./audio-engine"
import * as db from "./indexed-db"
import { WEBNATIVE_STAGING_ENV, WEBNATIVE_STAGING_MODE, debounce, fileExtension } from "./common"


// 🔐


// Redirect to HTTPS if using the `diffuse.sh` domain (subdomains included)
if (location.hostname.endsWith("diffuse.sh") && location.protocol === "http:") {
  location.href = location.href.replace("http://", "https://")
  failure("Just a moment, redirecting to HTTPS.")

// Not a secure context
} else if (!self.isSecureContext) {
  failure(`
    This app only works on a <a class="underline" target="_blank" href="https://developer.mozilla.org/en-US/docs/Web/Security/Secure_Contexts#When_is_a_context_considered_secure">secure context</a>, HTTPS & localhost, and modern browsers.
  `)

// Service worker
} else if ("serviceWorker" in navigator) {
  window.addEventListener("load", () => {
    navigator.serviceWorker
      .register("service-worker.js")
      .then(initialise)
      .catch(err => {
        const isFirefox = navigator.userAgent.toLowerCase().includes("firefox")

        console.error(err)
        return failure(
          location.protocol === "https:" || location.hostname === "localhost"
            ? "Failed to start the service worker." + (isFirefox ? " Make sure the setting <strong>Delete cookies and site data when Firefox is closed</strong> is off, or Diffuse's domain is added as an exception." : "")
            : "Failed to start the service worker, try using HTTPS."
        )
      })
  })

}



// 🍱


let app
let wire = {}


function initialise(reg) {
  app = Elm.UI.init({
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

  // ⚡️
  wire.brain()
  wire.audio()
  wire.backdrop()
  wire.clipboard()
  wire.covers()
  wire.serviceWorker(reg)
  wire.webnative()

  // Other ports
  app.ports.openUrlOnNewPage.subscribe(url => {
    window.open(url, "_blank")
  })
}


function failure(text) {
  const note = document.createElement("div")

  note.className = "flex flex-col font-body items-center h-screen italic justify-center leading-relaxed px-4 text-center text-base text-white"
  note.innerHTML = `
    <a class="block logo mb-5" href="../">
      <img src="../images/diffuse-light.svg" />
    </a>

    <p class="max-w-sm opacity-60">
      ${text}
    </p>
  `

  document.body.appendChild(note)

  // Remove loader
  const elm = document.querySelector("#elm")
  elm && elm.parentNode.removeChild(elm)
}



// Brain
// =====

let brain


wire.brain = () => {
  brain = new Worker(
    "brain.js#appHref=" +
    encodeURIComponent(window.location.href)
  )

  brain.onmessage = event => {
    if (event.data.action) return handleAction(event.data.action, event.data.data, event.ports)
    if (event.data.tag) app.ports.fromAlien.send(event.data)

    switch (event.data.tag) {
      case "GOT_CACHED_COVER": return gotCachedCover(event.data.data)
    }
  }

  app.ports.toBrain.subscribe(a => brain.postMessage(a))
}


function handleAction(action, data, _ports) { switch (action) {
  case "DOWNLOAD_TRACKS": return downloadTracks(data)
  case "FINISHED_DOWNLOADING_ARTWORK": return finishedDownloadingArtwork()
}}



// Audio
// -----

let orchestrion


wire.audio = () => {
  orchestrion = {
    activeQueueItem: null,
    audio: null,
    app: app,
    repeat: false
  }

  audioEngine.setup(orchestrion)

  app.ports.activeQueueItemChanged.subscribe(activeQueueItemChanged)
  app.ports.adjustEqualizerSetting.subscribe(adjustEqualizerSetting)
  app.ports.pause.subscribe(pause)
  app.ports.play.subscribe(play)
  app.ports.preloadAudio.subscribe(preloadAudio())
  app.ports.seek.subscribe(seek)
  app.ports.setRepeat.subscribe(setRepeat)
}


function activeQueueItemChanged(item) {
  if (
    orchestrion.activeQueueItem &&
    orchestrion.audio &&
    item &&
    item.trackId === orchestrion.activeQueueItem.trackId
  ) {
    orchestrion.audio.currentTime = 0
    return
  }

  const timestampInMilliseconds = Date.now()

  orchestrion.activeQueueItem = item
  orchestrion.audio = null
  orchestrion.coverPrep = null

  // Reset scrobble timer
  if (orchestrion.scrobbleTimer) {
    orchestrion.scrobbleTimer.stop()
    orchestrion.scrobbleTimer = null
  }

  // Remove older audio elements if possible
  audioEngine.usesSingleAudioNode()
    ? false
    : audioEngine.removeOlderAudioElements(timestampInMilliseconds)

  // 🎵
  if (item) {
    const coverPrep = {
      cacheKey:       btoa(unescape(encodeURIComponent(item.trackTags.artist + " --- " + item.trackTags.album))),
      trackFilename:  item.trackPath.split("/").reverse()[0],
      trackPath:      item.trackPath,
      trackSourceId:  item.sourceId,
      variousArtists: "f"
    }

    albumCover(coverPrep.cacheKey).then(maybeCover => {
      maybeCover = maybeCover === "TRIED" ? null : maybeCover
      orchestrion.coverPrep = coverPrep

      audioEngine.insertTrack(
        orchestrion,
        item,
        maybeCover

      ).then(() => {
        if (!maybeCover) {
          if (!orchestrion.audio) return
          orchestrion.audio.waitingForArtwork = coverPrep.cacheKey
          loadAlbumCovers([ coverPrep ])
        } else {
          orchestrion.audio.waitingForArtwork = null
        }

      })
    })

  // ✋
  } else {
    app.ports.setAudioIsPlaying.send(false)
    app.ports.setAudioPosition.send(0)
    if (navigator.mediaSession) navigator.mediaSession.playbackState = "none"

  }
}


function adjustEqualizerSetting(e) {
  audioEngine.adjustEqualizerSetting(orchestrion, e.knob, e.value)
}


function pause(_) {
  if (orchestrion.audio) orchestrion.audio.pause()
}


function play(_) {
  if (orchestrion.audio) orchestrion.audio.play()
}


function preloadAudio() {
  return debounce(item => {
    // Wait 15 seconds to preload something.
    // This is particularly useful when quickly shifting through tracks,
    // or when moving things around in the queue.
    (audioEngine.usesSingleAudioNode() || item.isCached)
      ? false
      : audioEngine.preloadAudioElement(orchestrion, item)
  }, 15000)
}


function seek(percentage) {
  audioEngine.seek(orchestrion.audio, percentage)
}


function setRepeat(repeat) {
  orchestrion.repeat = repeat
}



// Authentication
// --------------

let wn


wire.webnative = () => {
  app.ports.webnativeRequest.subscribe(request => {
    loadWebnative().then(() => {
      self.webnativeElm.request({ app: app, request: request })
    })
  })
}


function loadWebnative() {
  if (wn) return Promise.resolve()

  return loadScript("vendor/webnative.min.js")
    .then(() => loadScript("vendor/webnative-elm.min.js"))
    .then(() => {
      wn = window.webnative

      if ([ "localhost", "nightly.diffuse.sh" ].includes(location.hostname)) {
        wn.setup.debug({ enabled: true })
      }

      if (WEBNATIVE_STAGING_MODE) {
        wn.setup.endpoints(WEBNATIVE_STAGING_ENV)
      }
    })
}



// Backdrop
// --------

wire.backdrop = () => {
  app.ports.pickAverageBackgroundColor.subscribe(pickAverageBackgroundColor)
}


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


function pickAverageBackgroundColor(src) {
  const img = document.querySelector(`img[src$="${src}"]`)

  if (img) {
    const avgColor = averageColorOfImage(img)
    app.ports.setAverageBackgroundColor.send(avgColor)
  }
}



// Clipboard
// ---------

wire.clipboard = () => {
  app.ports.copyToClipboard.subscribe(copyToClipboard)
}


function copyToClipboard(text) {

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

}



// Covers
// ------

wire.covers = () => {
  app.ports.loadAlbumCovers.subscribe(
    debounce(loadAlbumCoversFromDom, 500)
  )

  db.keys().then(cachedCovers)
}


function albumCover(coverKey) {
  return db.getFromIndex({ key: `coverCache.${coverKey}` })
}


function gotCachedCover({ key, url }) {
  const item = orchestrion.activeQueueItem

  if (item && orchestrion.coverPrep && key === orchestrion.coverPrep.key && url) {
    let artwork = [{ src: url }]

    if (typeof url !== "string") {
      artwork = [{
        src: URL.createObjectURL(url),
        type: url.type
      }]
    }

    navigator.mediaSession.metadata = new MediaMetadata({
      title: item.trackTags.title,
      artist: item.trackTags.artist,
      album: item.trackTags.album,
      artwork: artwork
    })
  }
}


function loadAlbumCoversFromDom({ coverView, list } = {}) {
  let nodes = []

  if (list) nodes = nodes.concat(Array.from(
    document.querySelectorAll("#diffuse__track-covers [data-key]")
  ))

  if (coverView) nodes = nodes.concat(Array.from(
    document.querySelectorAll("#diffuse__track-covers + div [data-key]")
  ))

  if (!nodes.length) return;

  const coverPrepList = nodes.map(node => ({
    cacheKey:       node.getAttribute("data-key"),
    trackFilename:  node.getAttribute("data-filename"),
    trackPath:      node.getAttribute("data-path"),
    trackSourceId:  node.getAttribute("data-source-id"),
    variousArtists: node.getAttribute("data-various-artists")
  }))

  return loadAlbumCovers(coverPrepList)
}


function loadAlbumCovers(coverPrepList) {
  return coverPrepList.reduce((acc, prep) => {
    return acc.then(arr => {
      return albumCover(prep.cacheKey).then(a => {
        if (!a) return arr.concat([ prep ])
        return arr
      })
    })

  }, Promise.resolve([])).then(withoutEarlierAttempts => {
    brain.postMessage({
      action: "DOWNLOAD_ARTWORK",
      data: withoutEarlierAttempts
    })

  })
}


// Send a dictionary of the cached covers to the app.
function cachedCovers(keys) {
  const cacheKeys = keys.filter(
    k => k.startsWith("coverCache.")
  )

  const cachePromise = cacheKeys.reduce((acc, key) => {
    return acc.then(cache => {
      return db.getFromIndex({ key: key }).then(blob => {
        const cacheKey = key.slice(11)

        if (blob && typeof blob !== "string") {
          cache[cacheKey] = URL.createObjectURL(blob)
        }

        return cache
      })
    })
  }, Promise.resolve({}))

  cachePromise.then(cache => {
    app.ports.insertCoverCache.send(cache)
    setTimeout(() => loadAlbumCoversFromDom({ list: true, coverView: true }), 500)
  })
}


function finishedDownloadingArtwork() {
  if (!orchestrion.audio || !orchestrion.audio.waitingForArtwork || !orchestrion.activeQueueItem) return

  albumCover(orchestrion.audio.waitingForArtwork).then(maybeArtwork => {
    audioEngine.setMediaSessionMetadata(orchestrion.activeQueueItem, maybeArtwork)
  })

  orchestrion.audio.waitingForArtwork = null
}



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


window.addEventListener("blur", event => {
  if (app && event.target === window) app.ports.lostWindowFocus.send(null)
})



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
    app.ports.requestPlay.send(null)
  })


  navigator.mediaSession.setActionHandler("pause", () => {
    app.ports.requestPause.send(null)
  })


  navigator.mediaSession.setActionHandler("previoustrack", () => {
    app.ports.requestPrevious.send(null)
  })


  navigator.mediaSession.setActionHandler("nexttrack", () => {
    app.ports.requestNext.send(null)
  })


  navigator.mediaSession.setActionHandler("seekbackward", event => {
    const audio = orchestrion.audio
    const seekOffset = event.seekOffset || 10
    if (audio) audio.currentTime = Math.max(audio.currentTime - seekOffset, 0)
  })


  navigator.mediaSession.setActionHandler("seekforward", event => {
    const audio = orchestrion.audio
    const seekOffset = event.seekOffset || 10
    if (audio) audio.currentTime = Math.min(audio.currentTime + seekOffset, audio.duration)
  })


  navigator.mediaSession.setActionHandler("seekto", event => {
    const audio = orchestrion.audio
    if (audio) audio.currentTime = event.seekTime
  })

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



// Service worker
// --------------

wire.serviceWorker = async (reg) => {
  if (reg.installing) console.log("🧑‍✈️ Service worker is installing")
  const initialInstall = reg.installing

  reg.addEventListener("updatefound", () => {
    const newWorker = reg.installing

    // No worker was installed yet, so we'll only want to track the state changes
    if (newWorker !== initialInstall) {
      console.log("🧑‍✈️ A new version of Diffuse is available")
    }

    newWorker.addEventListener("statechange", (e) => {
      console.log("🧑‍✈️ Service worker is", e.target.state)
    })
  })

  // Check for service worker updates and every hour after that
  reg.update()
  setInterval(() => reg.update(), 1 * 1000 * 60 * 60)
}



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
