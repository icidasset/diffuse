//
// | (• ◡•)| (❍ᴥ❍ʋ)
//
// The bit where we launch the Elm app,
// and connect the other bits to it.

import { } from "./index.d"

import "tocca"

import { debounce } from "throttle-debounce"
import { saveAs } from "file-saver"
// import JSZip from "jszip"

import * as audioEngine from "./audio-engine"
import { db, fileExtension, WEBNATIVE_CONFIG } from "./common"
import { transformUrl } from "./urls"
import { version } from "../../package.json"



// 🌸


const isNativeWrapper = location.host === "localhost:44999" || location.host === "127.0.0.1:44999"



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
      .getRegistrations()
      .then(async registrations => {
        const resp = await fetch(`${location.origin}?ping=1`).then(r => r.text()).then(a => a === "false" ? false : true)
        const serverIsOnline = navigator.onLine && resp

        if (isNativeWrapper) await Promise.all(
          registrations.map(r => r.unregister())
        )

        return serverIsOnline
      })
      .then(async serverIsOnline => {
        if (serverIsOnline) {
          return navigator.serviceWorker.register(
            "service-worker.js",
            { type: "module" }
          )
        }
      })
      .then(_ => {
        return navigator.serviceWorker.ready
      })
      .catch(err => {
        const isFirefox = navigator.userAgent.toLowerCase().includes("firefox")

        console.error(err)
        return failure(
          location.protocol === "https:" || location.hostname === "localhost"
            ? "Failed to start the service worker." + (isFirefox ? " Make sure the setting <strong>Delete cookies and site data when Firefox is closed</strong> is off, or Diffuse's domain is added as an exception." : "")
            : "Failed to start the service worker, try using HTTPS."
        )
      })
      .then(initialise)
      .catch(err => {
        console.error(err)
        return failure("<strong>Failed to start the application.</strong><br />See browser console for details.")
      })
  })

}



// 🍱


let app
let brain
let wire: any = {}


async function initialise(reg) {
  brain = new Worker(
    "./js/brain/index.js#appHref=" + encodeURIComponent(window.location.href),
    { type: "module" }
  )

  brain.addEventListener("error", err => {
    failure("<strong>Failed to load web worker.</strong><br />If you're using Firefox, you might need to upgrade your browser (version 113 and up) and set `dom.workers.modules.enabled` to `true` in `about:config`")
  })

  await new Promise(resolve => {
    brain.onmessage = event => {
      if (event.data.action === "READY") resolve(null)
    }
  })

  app = Elm.UI.init({
    node: document.getElementById("elm"),
    flags: {
      buildTimestamp: BUILD_TIMESTAMP,
      darkMode: preferredColorScheme().matches,
      initialTime: Date.now(),
      isInstallingServiceWorker: !!reg.installing,
      isOnline: navigator.onLine,
      upgrade: viableForUpgrade(),
      version,
      viewport: {
        height: window.innerHeight,
        width: window.innerWidth
      }
    }
  })

  // ⚡️
  wire.brain()
  wire.audio()
  wire.backdrop()
  wire.broadcastChannel()
  wire.clipboard()
  wire.covers()
  wire.serviceWorker(reg)
  wire.webnative()

  // Other ports
  app.ports.openUrlOnNewPage.subscribe(url => {
    if (globalThis.__TAURI__) {
      globalThis.__TAURI__.shell.open(
        url.includes("://") ? url : `${location.origin}/${url.replace(/^\.\//, "")}`
      )

    } else {
      window.open(url, "_blank")

    }
  })

  app.ports.reloadApp.subscribe(_ => {
    let timeout = setTimeout(() => {
      if (reg.waiting) reg.waiting.postMessage("skipWaiting")
      window.location.reload()
    }, 250)

    bc.addEventListener("message", event => {
      if (event.data === "PONG") {
        clearTimeout(timeout)
        alert("⚠️ You can only update the app when you have no more than one instance open.")
      }
    })

    bc.postMessage("PING")
  })
}


function failure(text: string) {
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
  elm?.parentNode?.removeChild(elm)
}



// Brain
// =====

wire.brain = () => {
  brain.onmessage = event => {
    if (event.data.action) return handleAction(event.data.action, event.data.data, event.ports)
    if (event.data.tag) app.ports.fromAlien.send(event.data)

    switch (event.data.tag) {
      case "GOT_CACHED_COVER": return gotCachedCover(event.data.data)
    }
  }

  app.ports.toBrain.subscribe(a => brain.postMessage(a))
}


function handleAction(action, data, _ports) {
  switch (action) {
    case "DOWNLOAD_TRACKS": return downloadTracks(data)
    case "FINISHED_DOWNLOADING_ARTWORK": return finishedDownloadingArtwork()
  }
}



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
  audioEngine.removeOlderAudioElements(timestampInMilliseconds)

  // 🎵
  if (item) {
    const coverPrep = {
      cacheKey: btoa(unescape(encodeURIComponent(item.trackTags.artist + " --- " + item.trackTags.album))),
      trackFilename: item.trackPath.split("/").reverse()[ 0 ],
      trackPath: item.trackPath,
      trackSourceId: item.sourceId,
      variousArtists: "f"
    }

    albumCover(coverPrep.cacheKey).then(maybeCover => {
      maybeCover = maybeCover === "TRIED" ? null : maybeCover
      orchestrion.coverPrep = coverPrep

      audioEngine.insertTrack(
        orchestrion,
        item,
        maybeCover as any

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
  if (orchestrion.audio) {
    audioEngine.playAudio(orchestrion.audio, orchestrion.activeQueueItem, app)
  }
}


function preloadAudio() {
  return debounce(15000, item => {
    // Wait 15 seconds to preload something.
    // This is particularly useful when quickly shifting through tracks,
    // or when moving things around in the queue.
    item.isCached
      ? false
      : audioEngine.preloadAudioElement(orchestrion, item)
  })
}


function seek(percentage) {
  audioEngine.seek(orchestrion, percentage)
}


function setRepeat(repeat) {
  orchestrion.repeat = repeat
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

  if (!ctx) return null

  ctx.drawImage(img, 0, 0)

  const imageData = ctx.getImageData(0, 0, canvas.width, canvas.height)
  const color = { r: 0, g: 0, b: 0 }

  for (let i = 0, l = imageData.data.length; i < l; i += 4) {
    color.r += imageData.data[ i ]
    color.g += imageData.data[ i + 1 ]
    color.b += imageData.data[ i + 2 ]
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



// Broadcast channel
// -----------------

let bc

wire.broadcastChannel = () => {
  bc = new BroadcastChannel(`Diffuse-${location.hostname}`)
  bc.addEventListener("message", event => {
    switch (event.data) {
      case "PING": return bc.postMessage("PONG")
    }
  })
}



// Clipboard
// ---------

wire.clipboard = () => {
  app.ports.copyToClipboard.subscribe(async text => {
    // TODO: Find a better solution for this
    const adjustedText = (() => {
      if (text.startsWith("dropbox://")) {
        return transformUrl(text, app)
      } else if (text.startsWith("google://")) {
        return transformUrl(text, app)
      } else {
        return text

      }
    })()

    navigator.clipboard.writeText(await adjustedText)
  })
}



// Covers
// ------

wire.covers = () => {
  app.ports.loadAlbumCovers.subscribe(
    debounce(500, loadAlbumCoversFromDom)
  )

  db().keys().then(cachedCovers)
}


function albumCover(coverKey) {
  return db().getItem(`coverCache.${coverKey}`)
}


function gotCachedCover({ key, url }) {
  const item = orchestrion.activeQueueItem

  if (item && orchestrion.coverPrep && key === orchestrion.coverPrep.key && url) {
    let artwork = [ { src: url, type: undefined } ]

    if (typeof url !== "string") {
      artwork = [ {
        src: URL.createObjectURL(url),
        type: url.type
      } ]
    }

    navigator.mediaSession.metadata = new MediaMetadata({
      title: item.trackTags.title,
      artist: item.trackTags.artist,
      album: item.trackTags.album,
      artwork: artwork
    })
  }
}


function loadAlbumCoversFromDom({ coverView, list }) {
  let nodes: HTMLElement[] = []

  if (list) nodes = nodes.concat(Array.from(
    document.querySelectorAll("#diffuse__track-covers [data-key]")
  ))

  if (coverView) nodes = nodes.concat(Array.from(
    document.querySelectorAll("#diffuse__track-covers + div [data-key]")
  ))

  if (!nodes.length) return;

  const coverPrepList = nodes.map(node => ({
    cacheKey: node.getAttribute("data-key"),
    trackFilename: node.getAttribute("data-filename"),
    trackPath: node.getAttribute("data-path"),
    trackSourceId: node.getAttribute("data-source-id"),
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
      return db().getItem(key).then(blob => {
        const cacheKey = key.slice(11)

        if (blob && typeof blob !== "string" && blob instanceof Blob) {
          cache[ cacheKey ] = URL.createObjectURL(blob)
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
  // const zip = new JSZip()
  // const folder = zip.folder("Diffuse - " + group.name)
  // if (!folder) throw new Error("Failed to create ZIP file")

  // return group.tracks.reduce(
  //   (acc, track) => {
  //     return acc
  //       .then(_ => transformUrl(track.url, app))
  //       .then(fetch)
  //       .then(r => {
  //         const mimeType = r.headers.get("content-type")
  //         const fileExt = fileExtension(mimeType) || "unknown"

  //         return r.blob().then(
  //           b => folder.file(track.filename + "." + fileExt, b)
  //         )
  //       })
  //   },
  //   Promise.resolve()

  // ).then(_ => zip.generateAsync({ type: "blob" })
  // ).then(zipFile => {
  //   saveAs(zipFile, "Diffuse - " + group.name + ".zip")
  //   app.ports.downloadTracksFinished.send(null)

  // })
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
  app.ports.setIsOnline.send(navigator.onLine)
}



// Media Keys
// ----------

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
  let pointerEvent: any = new MouseEvent(eventType, mouseEvent)
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
  let pointerEvent: any = new CustomEvent(eventType, {
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
    const section = document.body.querySelector("section")
    const isDragging = section && section.classList.contains("dragging-something")
    const node = isDragging && document.elementFromPoint(event.clientX, event.clientY)

    if (node && node != enteredElement) {
      enteredElement && enteredElement.dispatchEvent(mousePointerEvent("pointerleave", event))
      node.dispatchEvent(mousePointerEvent("pointerenter", event))
      enteredElement = node
    }
  })
}


// Simulate `pointerenter` and `pointerleave` event for touch devices
document.body.addEventListener("touchmove", event => {
  const section = document.body.querySelector("section")
  const isDragging = section && section.classList.contains("dragging-something")

  let touch = event.touches[ 0 ]
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

wire.serviceWorker = async (reg: ServiceWorkerRegistration) => {
  if (reg.installing) console.log("🧑‍✈️ Service worker is installing")
  const initialInstall = reg.installing

  initialInstall?.addEventListener("statechange", function () {
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



// Syncing
// -------

let wn


wire.webnative = () => {
  app.ports.authenticateWithFission.subscribe(async () => {
    const program = await webnativeProgram()
    await program.capabilities.request({
      returnUrl: location.origin + "?action=authenticate/fission"
    })
  })

  app.ports.collectFissionCapabilities.subscribe(() => {
    // Webnative should collect the capabilities for us,
    // if everything is valid, we'll receive a session.
    webnativeProgram().then(
      () => app.ports.collectedFissionCapabilities.send(null)
    ).catch(
      err => console.error(err)
    )
  })
}



async function webnativeProgram() {
  try {
    await loadWebnative()
  } catch (err) {
    console.error(err)
    throw new Error("Failed to load Webnative")
  }

  return wn.program({
    ...WEBNATIVE_CONFIG,
    fileSystem: { loadImmediately: false }
  })
}


async function loadWebnative() {
  if (wn) return
  const webnative = await import("webnative")
  wn = webnative
}



// Touch Device
// ------------

window.addEventListener("touchstart", function onFirstTouch() {
  if (!app) return
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
// TODO: Remove support for older browsers
//       Replaced by `dvh` CSS unit

setVerticalHeightUnit()


window.addEventListener("resize", () => {
  setTimeout(setVerticalHeightUnit, 0)
})


function setVerticalHeightUnit() {
  const vh = document.documentElement.clientHeight * 0.01
  document.documentElement.style.setProperty("--vh", `${vh}px`)
}
