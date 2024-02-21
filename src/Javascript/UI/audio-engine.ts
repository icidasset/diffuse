

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
