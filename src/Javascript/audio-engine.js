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

  // Reset progress-bar width
  setProgressBarWidth(0)

  // Resume audio context if it's suspended
  if (context.resume && context.state !== "running") {
    context.resume()
  }

  // Initial promise
  const initialPromise = queueItem.isCached
    ? getFromIndex({ key: queueItem.trackId, store: storeNames.tracks }).then(blobToDataURL)
    : transformUrl(queueItem.url)

  // Find or create audio node
  let audioNode

  initialPromise.then(url => {
    queueItem =
      Object.assign({}, queueItem, { url: url })

    if ((audioNode = audioElementsContainer.querySelector("audio")) && SINGLE_AUDIO_NODE) {
      audioNode.setAttribute("src", queueItem.url)
      audioNode.setAttribute("rel", queueItem.trackId)
      audioNode.play()

    } else if (audioNode = findExistingAudioElement(queueItem)) {
      audioNode.setAttribute("data-timestamp", Date.now())
      audioNode.context = context.createMediaElementSource(audioNode)
      audioNode.context.connect(volume)

      if (audioNode.readyState >= 4) {
        playAudio(audioNode)
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


function createAudioElement(orchestrion, queueItem, timestampInMilliseconds) {
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
  // already preloaded?
  if (findExistingAudioElement(queueItem)) return

  // audio element remains valid for 2 hours
  createAudioElement(
    orchestrion,
    queueItem,
    Date.now() + 1000 * 60 * 60 * 2
  )
}



// Audio events
// ------------

function audioErrorEvent(event) {
  console.error(
    `Audio error for '${ audioElementTrackId(event.target) }': ` +
    (event.target.error.message || "")
  )

  switch (event.target.error.code) {
    case event.target.error.MEDIA_ERR_ABORTED:
      console.error("You aborted the audio playback.")
      break
    case event.target.error.MEDIA_ERR_NETWORK:
      console.error("A network error caused the audio download to fail.")
      break
    case event.target.error.MEDIA_ERR_DECODE:
      console.error("The audio playback was aborted due to a corruption problem or because the video used features your browser did not support.")

      // If this error happens at the end of the track, skip to the next.
      // NOTE: Weird issue with Chrome
      if (event.target.duration && (event.target.currentTime / event.target.duration) > 0.975) {
        console.log("Moving on to the next track.")
        this.app.ports.activeQueueItemEnded.send(null)
      }
      break
    case event.target.error.MEDIA_ERR_SRC_NOT_SUPPORTED:
      console.error("The audio not be loaded, either because the server or network failed or because the format is not supported.")

      app.ports.showErrorNotification.send(
        navigator.onLine
          ? "I can't play this track because of a network error"
          : "I can't play this track because we're offline"
      )

      break
    default:
      console.error("An unknown error occurred.")
  }
}


function audioStalledEvent(event) {
  this.app.ports.setAudioIsLoading.send(true)
  this.app.ports.setAudioHasStalled.send(true)
  unstallAudio(event.target)
}


function audioTimeUpdateEvent(event) {
  if (isNaN(event.target.duration) || isNaN(event.target.currentTime)) {
    setProgressBarWidth(0)
  } else if (event.target.duration > 0) {
    setProgressBarWidth(event.target.currentTime / event.target.duration)
  }
}


function audioEndEvent(event) {
  if (this.repeat) {
    playAudio(event.target)
  } else {
    this.app.ports.activeQueueItemEnded.send(null)
  }
}


function audioLoading(event) {
  clearTimeout(orchestrion.loadingTimeoutId)
  this.loadingTimeoutId = setTimeout(() => {
    if (this.audio.readyState === 4) {
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
  if (event.target.paused) playAudio(event.target)
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


function blobToDataURL(blob) {
  return new Promise((resolve, reject) => {
    const r = new FileReader()
    r.onerror = reject
    r.onload = e => resolve(e.target.result)
    r.readAsDataURL(blob)
  })
}


function isActiveAudioElement(orchestrion, node) {
  if (!orchestrion.activeQueueItem || !node) return false;
  return orchestrion.activeQueueItem.trackId === audioElementTrackId(node)
}


function playAudio(element) {
  const promise = element.play() || Promise.resolve()

  promise.catch(err => {
    console.error("Could not play audio automatically. Please resume playback manually.")
  })
}


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
