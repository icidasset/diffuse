//
// Elm loader
// | (• ◡•)| (❍ᴥ❍ʋ)
//
// The bit where we launch the Elm app,
// and connect the other bits to it.


const app = Elm.UI.init({
  node: document.getElementById("elm"),
  flags: {
    initialTime: Date.now(),
    viewport: {
      height: window.innerHeight,
      width: window.innerWidth
    }
  }
})



// Brain
// =====

const brain = new Worker("/workers/brain.js")

app.ports.toBrain.subscribe(thing => {
  brain.postMessage(thing)
})

brain.onmessage = event => {
  app.ports.fromAlien.send(event.data)
}



// Audio
// -----

const orchestrion = {
  activeQueueItem: null,
  app: app,
  repeat: false
}


app.ports.activeQueueItemChanged.subscribe(item => {
  const timestampInMilliseconds = Date.now()

  orchestrion.activeQueueItem = item
  orchestrion.audio = null

  removeOlderAudioElements(timestampInMilliseconds)

  if (item) {
    insertTrack(orchestrion, item)
  } else {
    app.ports.setAudioIsPlaying.send(false)
    setProgressBarWidth(0)
  }
})


app.ports.adjustEqualizerSetting.subscribe(e => {
  let node

  switch (e.knob) {
    case "LOW"      : node = low; break
    case "MID"      : node = mid; break
    case "HIGH"     : node = high; break
    case "VOLUME"   : node = volume; break
  }

  node.gain.setValueAtTime(
    determineNodeGainValue(e.knob, e.value),
    context.currentTime
  )
})


app.ports.play.subscribe(_ => {
  if (orchestrion.audio) orchestrion.audio.play()
})


app.ports.pause.subscribe(_ => {
  if (orchestrion.audio) orchestrion.audio.pause()
})


app.ports.seek.subscribe(percentage => {
  const audio = orchestrion.audio

  if (audio && !isNaN(audio.duration)) {
    audio.currentTime = audio.duration * percentage
    if (audio.paused) audio.pause()
  }
})


app.ports.setRepeat.subscribe(repeat => {
  orchestrion.repeat = repeat
})


app.ports.unstall.subscribe(_ => {
  if (orchestrion.audio) {
    clearTimeout(orchestrion.unstallTimeoutId)
    unstallAudio(orchestrion.audio)
  }
})



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



// Media Keys
// ----------

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
