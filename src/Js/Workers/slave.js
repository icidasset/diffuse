//
// Slave worker
// (◡ ‿ ◡ ✿)
//
// This worker is responsible for executing CPU-heavy tasks with the help of Elm.

importScripts("/vendor/package.js");
importScripts("/slave.js");


const app = Elm.Slave.worker();


//
// Incoming messages

self.onmessage = event => {
  console.log(event.data.aura);
  app.ports.incoming.send(event.data.aura);
};



//
// Slave ports

// app.processTags
