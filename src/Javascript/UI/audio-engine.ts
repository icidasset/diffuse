//
// Audio engine
// ♪(´ε｀ )
//
// Creates audio elements and interacts with the Web Audio API.


import { throttle } from "throttle-debounce"
import Timer from "timer.js"



// Audio events
// ------------

let showedNoNetworkError = false
let timesStalled = 1


function audioErrorEvent(event) {
  this.app.ports.setAudioIsPlaying.send(false)

  // MediaError

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
      } else if (navigator.onLine) {
        showUnsupportedSrcErrorNotification.call(this)
        clearTimeout(this.loadingTimeoutId)
        this.app.ports.setAudioIsLoading.send(false)
      } else {
        showNetworkErrorNotification.call(this)
        audioStalledEvent.call(this, event)
      }
      break
    default:
      console.error("An unknown error occurred.")
  }
}


function showNetworkErrorNotification() {
  if (showedNoNetworkError) return
  showedNoNetworkError = true
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
  this.unstallTimeout = setTimeout(_ => {
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
    try {
      navigator.mediaSession.setPositionState({
        duration: node.duration,
        position: node.currentTime
      })
    } catch (_err) { }
  }

  const progress = node.currentTime / node.duration

  if (node.duration >= 30 * 60) {
    sendProgress(this, progress)
  }
}


function audioEndEvent(event) {
  if (this.repeat) {
    event.target.startedPlayingAt = Math.floor(Date.now() / 1000)
    if (this.scrobbleTimer) this.scrobbleTimer.stop()
    playAudio(event.target, this.activeQueueItem, this.app)
  } else {
    this.app.ports.noteProgress.send({ trackId: this.activeQueueItem.trackId, progress: 1 })
    this.app.ports.activeQueueItemEnded.send(null)
  }
}


function audioLoading(event) {
  clearTimeout(this.loadingTimeoutId)

  this.loadingTimeoutId = setTimeout(() => {
    const audio = event.target

    if (!audio || !isActiveAudioElement(this, audio)) {
      return
    } else if (audio.readyState === 4 && audio.currentTime === 0) {
      this.app.ports.setAudioIsLoading.send(false)
    } else if (audio.readyState < 3 && IS_SAFARI) {
      this.app.ports.setAudioIsLoading.send(true)
      this.unstallTimeout = setTimeout(
        () => {
          if (isActiveAudioElement(this, audio)) {
            unstallSafariAudio.call(this, audio)
          }
        },
        timesStalled * 2500
      )
    } else {
      this.app.ports.setAudioIsLoading.send(true)
    }
  }, 1750)
}


function audioLoaded(event) {
  clearTimeout(this.loadingTimeoutId)
  clearTimeout(this.unstallTimeout)
  this.app.ports.setAudioHasStalled.send(false)
  this.app.ports.setAudioIsLoading.send(false)
  if (event.target.paused && (event.type === "seeked" || !event.target.hasPlayed)) {
    playAudio(event.target, this.activeQueueItem, this.app)
  }
}


function audioPlayEvent(event) {
  event.target.hasPlayed = true
  this.app.ports.setAudioIsPlaying.send(true)
  if (navigator.mediaSession) navigator.mediaSession.playbackState = "playing"
  if (this.scrobbleTimer) this.scrobbleTimer.start()
}


function audioPauseEvent(event) {
  this.app.ports.setAudioIsPlaying.send(false)
  if (navigator.mediaSession) navigator.mediaSession.playbackState = "paused"
  if (this.scrobbleTimer) this.scrobbleTimer.pause()
}






const sendProgress = throttle(30000, (orchestrion, progress) => {
  orchestrion.app.ports.noteProgress.send({
    trackId: orchestrion.activeQueueItem.trackId,
    progress: progress
  })
}, {
  noLeading: false,
  noTrailing: false
})


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

  audio.startedPlayingAt = timestamp

  this.scrobbleTimer = new Timer({
    onend: _ => this.app.ports.scrobble.send({
      duration: Math.round(lastSetDuration),
      timestamp: audio.startedPlayingAt || timestamp,
      trackId: trackId
    })
  })

  this.scrobbleTimer.start(scrobbleTimeoutDuration)
}


export function setMediaSessionMetadata(queueItem, maybeArtwork) {
  if ("mediaSession" in navigator === false || !queueItem.trackTags) return

  let artwork: MediaImage[] = []

  if (maybeArtwork && typeof maybeArtwork !== "string") {
    artwork = [ {
      src: URL.createObjectURL(maybeArtwork),
      type: maybeArtwork.type
    } ]
  }

  navigator.mediaSession.metadata = new MediaMetadata({
    title: queueItem.trackTags.title,
    artist: queueItem.trackTags.artist,
    album: queueItem.trackTags.album,
    artwork: artwork
  })
}


function unstallAudio(node: HTMLAudioElement) {
  const time = node.currentTime

  node.load()
  node.currentTime = time

  if (timesStalled > 5 && !showedNoNetworkError && navigator.onLine) {
    this.app.ports.showStickyErrorNotification.send(
      "You loaded too many tracks too quickly, " +
      "which the browser can't handle. " +
      "You'll most likely have to reload the browser."
    )
  }
}
