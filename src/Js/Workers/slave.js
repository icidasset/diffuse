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
  app.ports.incoming.send(event.data.aura);
};



//
// Slave ports

app.ports.outgoing.subscribe(aura => {
  switch (aura.action) {
    case "GET_TAGS":
      // TODO
      // nextAura = Object.assign({}, aura, { data: ... });
      // app.ports.incoming.send(nextAura);
      break;

    default:
      self.postMessage(aura);
  }
});
