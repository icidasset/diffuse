//
// Elm loader
// | (• ◡•)| (❍ᴥ❍ʋ)
//
// The bit where we launch the Elm app,
// and connect the other bits to it.


const app = Elm.UI.init({
  node: document.getElementById("elm"),
  flags: {}
})



// Brain
// =====

const brain = new Worker("/workers/brain.js")

app.ports.toBrain.subscribe(thing => {
  brain.postMessage(thing)
})

brain.onmessage = event => {
  app.ports.fromBrain.send(event.data)
}



// Audio
// -----

const orchestrion = {
  activeQueueItem: null,
  app: app,
  repeat: false
}


// app.ports.activeQueueItemChanged.subscribe(item => {
//   const timestampInMilliseconds = Date.now()
//
//   orchestrion.activeQueueItem = item
//   orchestrion.audio = null
//
//   removeOlderAudioElements(timestampInMilliseconds)
//
//   if (item) {
//     insertTrack(orchestrion, item)
//   } else {
//     app.ports.setIsPlaying.send(false)
//     setProgressBarWidth(0)
//   }
// })
//
//
// app.ports.play.subscribe(_ => {
//   if (orchestrion.audio) orchestrion.audio.play()
// })
//
//
// app.ports.pause.subscribe(_ => {
//   if (orchestrion.audio) orchestrion.audio.pause()
// })
//
//
// app.ports.seek.subscribe(percentage => {
//   const audio = orchestrion.audio
//
//   if (audio && !isNaN(audio.duration)) {
//     audio.currentTime = audio.duration * percentage
//     if (audio.paused) audio.pause()
//   }
// })
//
//
// app.ports.unstall.subscribe(_ => {
//   if (orchestrion.audio) {
//     clearTimeout(orchestrion.unstallTimeoutId)
//     unstallAudio(orchestrion.audio)
//   }
// })
