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

// TODO
