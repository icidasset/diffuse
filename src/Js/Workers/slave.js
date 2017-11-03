//
// Slave worker
// (◡ ‿ ◡ ✿)
//
// This worker is responsible for executing CPU-heavy tasks with the help of Elm.

importScripts("/vendor/package.js");
importScripts("/lib/processing.js");
importScripts("/slave.js");


const app = Elm.Slave.worker();


//
// Incoming messages

self.onmessage = event => {
  app.ports.incoming.send(event.data.aura);
};



//
// Slave ports

app.ports.outgoing.subscribe(aura => {
  self.postMessage(aura);
});


app.ports.requestTags.subscribe(context => {
  processContext(context).then(app.ports.receiveTags.send);
});
