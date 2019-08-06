//
// Audio engine
// ♪(´ε｀ )
//
// Creates audio elements and interacts with the Web Audio API.



// Audio context
// -------------

let context

if (window.AudioContext) {
  context = new window.AudioContext()
} else if (window.webkitAudioContext) {
  context = new window.webkitAudioContext()
}


const SINGLE_AUDIO_NODE = !!navigator.platform.match(/iPhone|iPod|iPad/)



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



// Playback
// --------

function insertTrack(orchestrion, queueItem) {
  if (!queueItem.url) console.error("insertTrack, missing `url`");
  if (!queueItem.trackId) console.error("insertTrack, missing `trackId`");

  // reset
  orchestrion.app.ports.setAudioHasStalled.send(false)
  clearTimeout(orchestrion.unstallTimeout)
  setProgressBarWidth(0)
  timesStalled = 0

  // resume audio context if it's suspended
  if (context.resume && context.state !== "running") {
    context.resume()
  }

  // initial promise
  const initialPromise = queueItem.isCached
    ? getFromIndex({ key: queueItem.trackId, store: storeNames.tracks }).then(blobUrl)
    : transformUrl(queueItem.url)

  // find or create audio node
  let audioNode

  initialPromise.then(url => {
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
        playAudio(audioNode, queueItem)
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

  const timeUpdateFunc = bind(audioTimeUpdateEvent)

  audio = new Audio()
  audio.setAttribute("crossorigin", "anonymous")
  audio.setAttribute("preload", "auto")
  audio.setAttribute("src", queueItem.url)
  audio.setAttribute("rel", queueItem.trackId)
  audio.setAttribute("data-timestamp", timestampInMilliseconds)
  audio.setAttribute("data-preload", isPreload ? "t" : "f")

  audio.crossorigin = "anonymous"
  audio.volume = 1

  audio.addEventListener("error", bind(audioErrorEvent))
  audio.addEventListener("stalled", bind(audioStalledEvent))

  audio.addEventListener("canplay", bind(audioCanPlayEvent))
  audio.addEventListener("ended", bind(audioEndEvent))
  audio.addEventListener("loadstart", bind(audioLoading))
  audio.addEventListener("loadeddata", bind(audioLoaded))
  audio.addEventListener("pause", bind(audioPauseEvent))
  audio.addEventListener("play", bind(audioPlayEvent))
  audio.addEventListener("seeking", bind(audioLoading))
  audio.addEventListener("seeked", bind(audioLoaded))
  audio.addEventListener("timeupdate", timeUpdateFunc)

  audio.load()
  audioElementsContainer.appendChild(audio)

  return audio
}


function preloadAudioElement(orchestrion, queueItem) {
  // already loaded?
  if (findExistingAudioElement(queueItem)) return

  // remove other preloads
  audioElementsContainer.querySelectorAll(`[data-preload="t"]`).forEach(
    n => n.parentNode.removeChild(n)
  )

  // audio element remains valid for 2 hours
  createAudioElement(
    orchestrion,
    queueItem,
    Date.now() + 1000 * 60 * 60 * 2,
    true
  )
}



// Audio events
// ------------

let timesStalled = 0


function audioErrorEvent(event) {
  this.app.ports.setAudioIsPlaying.send(false)

  switch (event.target.error.code) {
    case event.target.error.MEDIA_ERR_ABORTED:
      console.error("You aborted the audio playback.")
      break
    case event.target.error.MEDIA_ERR_NETWORK:
      console.error("A network error caused the audio download to fail.")

      app.ports.showErrorNotification.send(
        navigator.onLine
          ? "I can't play this track because of a network error"
          : "I can't play this track because we're offline"
      )
      break
    case event.target.error.MEDIA_ERR_DECODE:
      console.error("The audio playback was aborted due to a corruption problem or because the video used features your browser did not support.")
      break
    case event.target.error.MEDIA_ERR_SRC_NOT_SUPPORTED:
      console.error("The audio not be loaded, either because the server or network failed or because the format is not supported.")

      audioStalledEvent.call(this, event)
      break
    default:
      console.error("An unknown error occurred.")
  }
}


function audioStalledEvent(event) {
  this.app.ports.setAudioIsLoading.send(true)
  this.app.ports.setAudioHasStalled.send(true)

  clearTimeout(this.unstallTimeout)

  // Timeout
  setTimeout(_ => {
    if (isActiveAudioElement(this, event.target)) {
      unstallAudio(event.target)
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
    node.duration === 0) {
    return setProgressBarWidth(0)
  }

  const progress = node.currentTime / node.duration

  setProgressBarWidth(progress)

  if (node.duration >= 30 * 60) {
    sendProgress(this, progress)
  }
}


function audioEndEvent(event) {
  if (this.repeat) {
    playAudio(event.target, this.activeQueueItem)
  } else {
    this.app.ports.noteProgress.send({ trackId: this.activeQueueItem.trackId, progress: 1 })
    this.app.ports.activeQueueItemEnded.send(null)
  }
}


function audioLoading(event) {
  clearTimeout(orchestrion.loadingTimeoutId)

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
  if (event.target.paused) playAudio(event.target, this.activeQueueItem)
}


function audioPlayEvent(event) {
  this.app.ports.setAudioIsPlaying.send(true)
}


function audioPauseEvent(event) {
  this.app.ports.setAudioIsPlaying.send(false)
}


let lastSetDuration = 0


function audioCanPlayEvent(event) {
  if (event.target.duration != lastSetDuration) {
    this.app.ports.setAudioDuration.send(event.target.duration || 0)
    lastSetDuration = event.target.duration
  }
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


function playAudio(element, queueItem) {
  if (queueItem.progress && element.duration) {
    element.currentTime = queueItem.progress * element.duration
  }

  const promise = element.play() || Promise.resolve()

  promise.catch(err => {
    console.error("Could not play audio automatically. Please resume playback manually.")
  })
}


const sendProgress = throttle((orchestrion, progress) => {
  orchestrion.app.ports.noteProgress.send({
    trackId: orchestrion.activeQueueItem.trackId,
    progress: progress
  })
}, 30000)


function unstallAudio(node) {
  const time = node.currentTime

  node.load()
  node.currentTime = time
}



// Progress Bar
// ------------

let progressBarNode

function setProgressBarWidth(float) {
  if (!progressBarNode || !progressBarNode.offsetParent) {
    progressBarNode = document.querySelector(".progressBarValue")
  }

  if (progressBarNode) {
    progressBarNode.style.width = (float * 100).toString() + "%"
  }
}



// 💥
// --
// Remove all the audio elements with a timestamp older than the given one.

function removeOlderAudioElements(timestamp) {
  const nodes = audioElementsContainer.querySelectorAll("audio[data-timestamp]")

  nodes.forEach(node => {
    const t = parseInt(node.getAttribute("data-timestamp"), 10)
    if (t >= timestamp) return

    if (node.context) {
      node.context.disconnect()
      node.context = null
    }

    audioElementsContainer.removeChild(node)
  })
}
