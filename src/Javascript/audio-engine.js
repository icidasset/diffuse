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


document.body.appendChild(audioElementsContainer)



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
    case "Volume"   : return value
    default         : return value < 0 ? value * 50 : value * 15
  }
}



// Playback
// --------

function insertTrack(orchestrion, queueItem) {
  if (!queueItem.url) console.error("insertTrack, missing `url`");
  if (!queueItem.track && !queueItem.track.id) console.error("insertTrack, missing `track.id`");

  // Resume audio context if it's suspended
  if (context.resume && context.state !== "running") {
    context.resume()
  }

  // Create audio node
  let audioNode

  audioNode = createAudioElement(orchestrion, queueItem)
  audioNode.context = context.createMediaElementSource(audioNode)
  audioNode.context.connect(volume)
}


function createAudioElement(orchestrion, queueItem) {
  let audio
  let timestampInMilliseconds = Date.now()

  const bind = fn => event => {
    const is = isActiveAudioElement(orchestrion, event.target)
    if (is) fn.call(orchestrion, event)
  }

  const timeUpdateFunc = bind(audioTimeUpdateEvent)

  audio = new window.Audio()
  audio.setAttribute("crossOrigin", "anonymous")
  audio.setAttribute("crossorigin", "anonymous")
  audio.setAttribute("preload", "none")
  audio.setAttribute("src", queueItem.url)
  audio.setAttribute("rel", queueItem.track.id)
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
  orchestrion.audio = audio

  return audio
}



// Audio events
// ------------

function audioErrorEvent(event) {
  console.error(`Audio error for '${ audioElementTrackId(event.target) }'`)

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
        // TODO
        // this.app.ports.activeQueueItemEnded.send(null)
      }
      break
    case event.target.error.MEDIA_ERR_SRC_NOT_SUPPORTED:
      console.error("The audio not be loaded, either because the server or network failed or because the format is not supported.")
      break
    default:
      console.error("An unknown error occurred.")
  }
}


function audioStalledEvent(event) {
  this.stalledTimeoutId = setTimeout(() => {
    console.error(`Audio stalled for '${ audioElementTrackId(event.target) }'`)

    // TODO:
    // this.app.ports.setStalled.send(true)
    this.unstallTimeoutId = setTimeout(() => {
      // this.app.ports.setStalled.send(false)
      unstallAudio(event.target)
    }, 2500)
  }, 60000)
}


function audioTimeUpdateEvent(event) {
  clearTimeout(this.stalledTimeoutId)

  if (isNaN(event.target.duration) || isNaN(event.target.currentTime)) {
    setProgressBarWidth(0)
  } else if (event.target.duration > 0) {
    setProgressBarWidth(event.target.currentTime / event.target.duration)
  }
}


function audioEndEvent(event) {
  if (this.repeat) {
    event.target.play()
  } else {
    // TODO: this.app.ports.activeQueueItemEnded.send(null)
  }
}


function audioLoading() {
  this.loadingTimeoutId = setTimeout(() => {
    // TODO: this.app.ports.setIsLoading.send(true)
  }, 1750)
}


function audioLoaded() {
  clearTimeout(this.loadingTimeoutId)
  // TODO: this.app.ports.setIsLoading.send(false)
}


function audioPlayEvent(event) {
  // TODO: this.app.ports.setIsPlaying.send(true)
}


function audioPauseEvent(event) {
  // TODO: this.app.ports.setIsPlaying.send(false)
}


let lastSetDuration = 0


function audioCanPlayEvent(event) {
  if (event.target.paused) event.target.play()
  if (event.target.duration != lastSetDuration) {
    // TODO:
    // this.app.ports.setDuration.send(event.target.duration || 0)
    lastSetDuration = event.target.duration
  }
}



// 🖍 Utensils
// -----------

function audioElementTrackId(node) {
  return node ? node.getAttribute("rel") : undefined
}


function isActiveAudioElement(orchestrion, node) {
  if (!orchestrion.activeQueueItem || !node) return false;
  return orchestrion.activeQueueItem.track.id === audioElementTrackId(node)
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

    node.context.disconnect()
    node.context = null

    audioElementsContainer.removeChild(node)
  })
}
