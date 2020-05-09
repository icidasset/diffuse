//
// Audio engine
// ♪(´ε｀ )
//
// Creates audio elements and interacts with the Web Audio API.


import Timer from "timer.js"
import * as db from "./indexed-db"
import { throttle } from "./common"
import { transformUrl } from "./urls"


// Audio context
// -------------

var context

if (window.AudioContext) {
  context = new window.AudioContext()
} else if (window.webkitAudioContext) {
  context = new window.webkitAudioContext()
}

self.context = context


let SINGLE_AUDIO_NODE = !!navigator.platform.match(/iPhone|iPod|iPad/) ||
                        !!navigator.userAgent.includes("AppleWebKit")


export function usesSingleAudioNode() {
  return SINGLE_AUDIO_NODE
}



// Container for <audio> elements
// ------------------------------

const audioElementsContainer = (() => {
  let c
  let styles =
    [ "height: 0"
    , "width: 0"
    , "visibility: hidden"
    , "pointer-events: none"
    ]

  c = document.createElement("div")
  c.setAttribute("class", "absolute left-0 top-0")
  c.setAttribute("style", styles.join("; "))

  return c
})()


function addAudioContainer() {
  document.body.appendChild(audioElementsContainer)
}



// Setup
// -----

const silentMp3File = "data:audio/mp3;base64,SUQzBAAAAAAAI1RTU0UAAAAPAAADTGF2ZjU2LjM2LjEwMAAAAAAAAAAAAAAA//OEAAAAAAAAAAAAAAAAAAAAAAAASW5mbwAAAA8AAAAEAAABIADAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDV1dXV1dXV1dXV1dXV1dXV1dXV1dXV1dXV6urq6urq6urq6urq6urq6urq6urq6urq6v////////////////////////////////8AAAAATGF2YzU2LjQxAAAAAAAAAAAAAAAAJAAAAAAAAAAAASDs90hvAAAAAAAAAAAAAAAAAAAA//MUZAAAAAGkAAAAAAAAA0gAAAAATEFN//MUZAMAAAGkAAAAAAAAA0gAAAAARTMu//MUZAYAAAGkAAAAAAAAA0gAAAAAOTku//MUZAkAAAGkAAAAAAAAA0gAAAAANVVV"


export function setup(orchestrion) {
  addAudioContainer()

  if (SINGLE_AUDIO_NODE) {
    // Try to avoid the "couldn't play automatically" error,
    // which seems to happen with audio nodes using an url created by `createObjectURL`.
    insertTrack(orchestrion, { url: silentMp3File, trackId: "" }).then(_ => {
      const temporaryClickHandler = () => {
        orchestrion.audio.play()
        document.body.removeEventListener("click", temporaryClickHandler)
      }

      document.body.addEventListener("click", temporaryClickHandler)
    })
  }
}



// Audio nodes
// -----------
// Flow:
// {Input} -> Volume -> Low -> Mid -> High -> {Output}

let volume,
    low,
    mid,
    high

// volume
volume = context.createGain()
volume.gain.value = 1

// biquad filters
low   = context.createBiquadFilter()
mid   = context.createBiquadFilter()
high  = context.createBiquadFilter()

low.type    = "lowshelf"
mid.type    = "peaking"
high.type   = "highshelf"

low.frequency.value   = 250
mid.frequency.value   = 2750
mid.Q.value           = 1
high.frequency.value  = 8000

// connect them nodes
volume.connect(low)
low.connect(mid)
mid.connect(high)
high.connect(context.destination)


function determineNodeGainValue(knobType, value) {
  switch (knobType) {
    case "VOLUME"   : return value
    default         : return value < 0 ? value * 50 : value * 15
  }
}


export function adjustEqualizerSetting(knobType, value) {
  let node

  switch (knobType) {
    case "LOW"      : node = low; break
    case "MID"      : node = mid; break
    case "HIGH"     : node = high; break
    case "VOLUME"   : node = volume; break
  }

  node.gain.setValueAtTime(
    determineNodeGainValue(knobType, value),
    context.currentTime
  )
}



// Playback
// --------

export function insertTrack(orchestrion, queueItem) {
  if (queueItem.url == undefined) console.error("insertTrack, missing `url`");
  if (queueItem.trackId == undefined) console.error("insertTrack, missing `trackId`");

  // reset
  orchestrion.app.ports.setAudioHasStalled.send(false)
  orchestrion.app.ports.setAudioPosition.send(0)
  clearTimeout(orchestrion.unstallTimeout)
  timesStalled = 1

  // resume audio context if it's suspended
  if (context.resume && context.state !== "running") {
    context.resume()
  }

  // metadata
  if ("mediaSession" in navigator && queueItem.trackTags) {
    navigator.mediaSession.metadata = new MediaMetadata({
      title: queueItem.trackTags.title,
      artist: queueItem.trackTags.artist,
      album: queueItem.trackTags.album
    })
  }

  // initial promise
  const initialPromise = queueItem.isCached
    ? db.getFromIndex({ key: queueItem.trackId, store: db.storeNames.tracks }).then(blobUrl)
    : transformUrl(queueItem.url)

  // find or create audio node
  let audioNode

  return initialPromise.then(url => {
    queueItem =
      Object.assign({}, queueItem, { url: url })

    if ((audioNode = audioElementsContainer.querySelector("audio")) && SINGLE_AUDIO_NODE) {
      audioNode.setAttribute("src", queueItem.url)
      audioNode.setAttribute("rel", queueItem.trackId)
      audioNode.load()

    } else if (audioNode = findExistingAudioElement(queueItem)) {
      audioNode.setAttribute("data-preload", "f")
      audioNode.setAttribute("data-timestamp", Date.now())
      audioNode.context = context.createMediaElementSource(audioNode)
      audioNode.context.connect(volume)

      if (audioNode.readyState >= 4) {
        playAudio(audioNode, queueItem, orchestrion.app)
      } else {
        orchestrion.app.ports.setAudioIsLoading.send(true)
        audioNode.load()
      }

    } else {
      audioNode = createAudioElement(orchestrion, queueItem, Date.now())
      audioNode.context = context.createMediaElementSource(audioNode)
      audioNode.context.connect(volume)

    }

    orchestrion.audio = audioNode
  })
}


function findExistingAudioElement(queueItem) {
  return audioElementsContainer.querySelector(`[rel="${queueItem.trackId}"]`)
}


function createAudioElement(orchestrion, queueItem, timestampInMilliseconds, isPreload) {
  let audio

  const bind = fn => event => {
    const is = isActiveAudioElement(orchestrion, event.target)
    if (is) fn.call(orchestrion, event)
  }

  const crossorigin = queueItem.url.includes("service_worker_authentication")
    ? "use-credentials"
    : "anonymous"

  audio = new Audio()
  audio.setAttribute("crossorigin", crossorigin)
  audio.setAttribute("data-preload", isPreload ? "t" : "f")
  audio.setAttribute("data-timestamp", timestampInMilliseconds)
  audio.setAttribute("preload", SINGLE_AUDIO_NODE ? "none" : "auto")
  audio.setAttribute("rel", queueItem.trackId)
  audio.setAttribute("src", queueItem.url)

  audio.crossorigin = "anonymous"
  audio.volume = 1

  audio.addEventListener("canplay", bind(audioCanPlayEvent))
  audio.addEventListener("ended", bind(audioEndEvent))
  audio.addEventListener("error", bind(audioErrorEvent))
  audio.addEventListener("loadstart", bind(audioLoading))
  audio.addEventListener("loadeddata", bind(audioLoaded))
  audio.addEventListener("pause", bind(audioPauseEvent))
  audio.addEventListener("play", bind(audioPlayEvent))
  audio.addEventListener("seeking", bind(audioLoading))
  audio.addEventListener("seeked", bind(audioLoaded))
  audio.addEventListener("stalled", bind(audioStalledEvent))
  audio.addEventListener("timeupdate", bind(audioTimeUpdateEvent))

  audio.load()
  audioElementsContainer.appendChild(audio)

  return audio
}


export function preloadAudioElement(orchestrion, queueItem) {
  // already loaded?
  if (findExistingAudioElement(queueItem)) return

  // remove other preloads
  audioElementsContainer.querySelectorAll(`[data-preload="t"]`).forEach(
    n => n.parentNode.removeChild(n)
  )

  // audio element remains valid for 2 hours
  transformUrl(queueItem.url).then(url => {
    const queueItemWithTransformedUrl =
      Object.assign({}, queueItem, { url: url })

    createAudioElement(
      orchestrion,
      queueItemWithTransformedUrl,
      Date.now() + 1000 * 60 * 60 * 2,
      true
    )
  })
}


export function seek(audio, percentage) {
  if (audio && !isNaN(audio.duration)) {
    if (audio.paused) audio.play()
    audio.currentTime = audio.duration * percentage
  }
}



// Audio events
// ------------

let timesStalled = 1


function audioErrorEvent(event) {
  this.app.ports.setAudioIsPlaying.send(false)

  switch (event.target.error.code) {
    case event.target.error.MEDIA_ERR_ABORTED:
      console.error("You aborted the audio playback.")
      break
    case event.target.error.MEDIA_ERR_NETWORK:
      console.error("A network error caused the audio download to fail.")
      showNetworkErrorNotification.call(this)
      audioStalledEvent.call(this, event)
      break
    case event.target.error.MEDIA_ERR_DECODE:
      console.error("The audio playback was aborted due to a corruption problem or because the video used features your browser did not support.")
      break
    case event.target.error.MEDIA_ERR_SRC_NOT_SUPPORTED:
      console.error("The audio not be loaded, either because the server or network failed or because the format is not supported.")
      if (event.target.currentTime && event.target.currentTime > 0) {
        showNetworkErrorNotification.call(this)
        audioStalledEvent.call(this, event)
      } else {
        showUnsupportedSrcErrorNotification.call(this)
        clearTimeout(this.loadingTimeoutId)
        this.app.ports.setAudioIsLoading.send(false)
      }
      break
    default:
      console.error("An unknown error occurred.")
  }
}


    function showNetworkErrorNotification() {
      this.app.ports.showErrorNotification.send(
        navigator.onLine
          ? "I can't play this track because of a network error. I'll try to reconnect."
          : "I can't play this track because we're offline. I'll try to reconnect."
      )
    }


    function showUnsupportedSrcErrorNotification() {
      this.app.ports.showErrorNotification.send(
        "__I can't play this track because your browser didn't recognize it.__ Try checking your developer console for a warning to find out why."
      )
    }


function audioStalledEvent(event, notifyAppImmediately) {
  this.app.ports.setAudioIsLoading.send(true)
  clearTimeout(this.unstallTimeout)

  // Notify app
  if (timesStalled >= 3 || notifyAppImmediately) {
    this.app.ports.setAudioHasStalled.send(true)
  }

  // Timeout
  setTimeout(_ => {
    if (isActiveAudioElement(this, event.target)) {
      unstallAudio.call(this, event.target)
    }
  }, timesStalled * 2500)

  // Increase counter
  timesStalled++
}


function audioTimeUpdateEvent(event) {
  const node = event.target

  if (
    isNaN(node.duration) ||
    isNaN(node.currentTime) ||
    node.duration === 0
  ) return;

  setDurationIfNecessary.call(this, node)
  this.app.ports.setAudioPosition.send(node.currentTime)

  if (navigator.mediaSession && navigator.mediaSession.setPositionState) {
    navigator.mediaSession.setPositionState({
      duration: node.duration,
      position: node.currentTime
    })
  }

  const progress = node.currentTime / node.duration

  if (node.duration >= 30 * 60) {
    sendProgress(this, progress)
  }
}


function audioEndEvent(event) {
  if (this.repeat) {
    playAudio(event.target, this.activeQueueItem, this.app)
  } else {
    this.app.ports.noteProgress.send({ trackId: this.activeQueueItem.trackId, progress: 1 })
    this.app.ports.activeQueueItemEnded.send(null)
  }
}


function audioLoading(_event) {
  clearTimeout(this.loadingTimeoutId)

  this.loadingTimeoutId = setTimeout(() => {
    if (this.audio.readyState === 4 && this.audio.currentTime === 0) {
      this.app.ports.setAudioIsLoading.send(false)
    } else {
      this.app.ports.setAudioIsLoading.send(true)
    }
  }, 1750)
}


function audioLoaded(event) {
  clearTimeout(this.loadingTimeoutId)
  this.app.ports.setAudioHasStalled.send(false)
  this.app.ports.setAudioIsLoading.send(false)
  if (event.target.paused) playAudio(event.target, this.activeQueueItem, this.app)
}


function audioPlayEvent(_event) {
  this.app.ports.setAudioIsPlaying.send(true)
  if (navigator.mediaSession) navigator.mediaSession.playbackState = "playing"
  if (this.scrobbleTimer) this.scrobbleTimer.start()
}


function audioPauseEvent(_event) {
  this.app.ports.setAudioIsPlaying.send(false)
  if (navigator.mediaSession) navigator.mediaSession.playbackState = "paused"
  if (this.scrobbleTimer) this.scrobbleTimer.pause()
}


function audioCanPlayEvent(event) {
  setDurationIfNecessary.call(this, event.target)
}



// 🖍 Utensils
// -----------

function audioElementTrackId(node) {
  return node ? node.getAttribute("rel") : undefined
}


function blobUrl(blob) {
  return URL.createObjectURL(blob)
}


function isActiveAudioElement(orchestrion, node) {
  return (
    !orchestrion.activeQueueItem ||
    !node ||
    node.getAttribute("data-preload") === "t"
  )
  ? false
  : orchestrion.activeQueueItem.trackId === audioElementTrackId(node)
}


function playAudio(element, queueItem, app) {
  if (queueItem.progress && element.duration) {
    element.currentTime = queueItem.progress * element.duration
  }

  const promise = element.play() || Promise.resolve()

  promise.catch(e => {
    SINGLE_AUDIO_NODE = true

    const err = "Couldn't play audio automatically. Please resume playback manually."
    console.error(err, e)
    if (app) app.ports.fromAlien.send({ tag: "", data: null, error: err })
  })
}


const sendProgress = throttle((orchestrion, progress) => {
  orchestrion.app.ports.noteProgress.send({
    trackId: orchestrion.activeQueueItem.trackId,
    progress: progress
  })
}, 30000)


let lastSetDuration = 0


function setDurationIfNecessary(audio) {
  if (audio.duration === lastSetDuration) return;

  this.app.ports.setAudioDuration.send(audio.duration || 0)
  lastSetDuration = audio.duration

  // Scrobble
  if (!lastSetDuration || lastSetDuration < 30) return;

  const timestamp = Math.floor(Date.now() / 1000)
  const scrobbleTimeoutDuration = Math.min(240 + 0.5, lastSetDuration / 1.95)
  const trackId = audio.getAttribute("rel")

  this.scrobbleTimer = new Timer({
    onend: _ => this.app.ports.scrobble.send({
      duration: Math.round(lastSetDuration),
      timestamp: timestamp,
      trackId: trackId
    })
  })

  this.scrobbleTimer.start(scrobbleTimeoutDuration)
}


function unstallAudio(node) {
  const time = node.currentTime

  node.load()
  node.currentTime = time

  if (timesStalled >= 6) {
    this.app.ports.showStickyErrorNotification.send(
      "You loaded too many tracks too quickly, " +
      "which the browser can't handle. " +
      "You'll most likely have to reload the browser."
    )
  }
}



// 💥
// --
// Remove all the audio elements with a timestamp older than the given one.

export function removeOlderAudioElements(timestamp) {
  const nodes = audioElementsContainer.querySelectorAll("audio[data-timestamp]")

  nodes.forEach(node => {
    const t = parseInt(node.getAttribute("data-timestamp"), 10)
    if (t >= timestamp) return

    if (node.context) {
      node.context.disconnect()
      node.context = null
    }

    // Force browser to stop loading
    node.src = silentMp3File

    // Remove element
    audioElementsContainer.removeChild(node)
  })
}
