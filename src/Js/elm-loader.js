//
// Elm loader
// | (• ◡•)| (❍ᴥ❍ʋ)
//
// The bit where we launch the Elm app,
// and connect the other bits to it.

let app, node;

node = document.getElementById("elm-container");
node.innerHTML = "";

app = Elm.App.embed(
  node,
  { isDevelopmentEnvironment: location.hostname === "localhost"
  , isElectron: !!self.electron
  , isHTTPS: location.protocol === "https:"
  , isOnline: navigator.onLine
  , screenHeight: window.screen.height || window.outerHeight
  }
);



//
// > Abroad

app.ports.importData.subscribe(id => {
  const node = document.getElementById(id);
  if (node == null) return;

  const file = node.files[0];
  const reader = new FileReader();

  reader.onload = event => {
    const json = event.target.result;
    app.ports.importDataReady.send(json);
  };

  reader.readAsText(file);
});



//
// > Audio

const audioEnvironmentContext = {
  activeQueueItem: null,
  elm: app,
  repeat: false
};

app.ports.activeQueueItemChanged.subscribe(item => {
  const timestampInMilliseconds = Date.now();

  audioEnvironmentContext.activeQueueItem = item;
  audioEnvironmentContext.audio = null;

  removeOlderAudioElements(timestampInMilliseconds);

  if (item) {
    insertTrack(audioEnvironmentContext, item);
  } else {
    app.ports.setIsPlaying.send(false);
    setProgressBarWidth(0);
  }
});

app.ports.requestPlay.subscribe(_ => {
  if (audioEnvironmentContext.audio) {
    audioEnvironmentContext.audio.play();
  }
});

app.ports.requestPause.subscribe(_ => {
  if (audioEnvironmentContext.audio) {
    audioEnvironmentContext.audio.pause();
  }
});

app.ports.requestSeek.subscribe(percentage => {
  const audio = audioEnvironmentContext.audio;

  if (audio && !isNaN(audio.duration)) {
    audio.currentTime = audio.duration * percentage;
    if (audio.paused) audio.pause();
  }
});

app.ports.requestUnstall.subscribe(_ => {
  const audio = audioEnvironmentContext.audio;

  if (audio) {
    clearTimeout(audioEnvironmentContext.unstallTimeoutId);
    unstallAudio(audio);
  }
});



//
// > Authentication

app.ports.authenticationEvent.subscribe(event => {
  let funcName, method;
  let report = app.ports.authenticationEventResult.send;

  switch (event.tag) {

    case "METHOD_GET":
    case "METHOD_SET":
    case "METHOD_UNSET":
      // Call function on the METHOD object.
      // For example: METHOD_GET -> METHOD.get()
      funcName = camelcase(event.tag.replace(/^\w+_/, ""));

      report({
        tag:    event.tag,
        data:   AUTH_SYSTEM.METHOD[funcName](event.data),
        error:  null
      });

      break;

    default:
      // Perform action on the active authentication method.
      // For example: CONSTRUCT -> LOCAL.construct()
      method    = AUTH_SYSTEM.METHOD.get();
      funcName  = camelcase(event.tag);

      AUTH_SYSTEM[method][funcName](event.data).then(
        data => report({ tag: event.tag, data: data, error: null }),
        err  => report({ tag: event.tag, data: null, error: err.toString() })
      );

  }
});


const LOCAL_USER_DATA_KEY = "additional-userdata";


app.ports.clearLocalUserData.subscribe(_ => {
  self.localStorage.removeItem(LOCAL_USER_DATA_KEY);
});


app.ports.retrieveLocalUserData.subscribe(_ => {
  const data = self.localStorage.getItem(LOCAL_USER_DATA_KEY);
  app.ports.retrievedLocalUserData.send(data);
});


app.ports.storeLocalUserData.subscribe(data => {
  self.localStorage.setItem(LOCAL_USER_DATA_KEY, data);
});



//
// > Equalizer

app.ports.adjustEqualizerSetting.subscribe(e => {
  let node;

  switch (e.knob) {
    case "Low"      : node = low; break;
    case "Mid"      : node = mid; break;
    case "High"     : node = high; break;
    case "Volume"   : node = volume; break;
  }

  node.gain.setValueAtTime(
    determineNodeGainValue(e.knob, e.value),
    context.currentTime
  );
});



//
// > Notifications

app.ports.fadeInNotifications.subscribe(_ => {
  setTimeout(
    () => document.querySelectorAll(".notification:not(.is-shown)").forEach(node => {
      window.getComputedStyle(node).opacity;
      node.classList.add("is-shown");
      node.style.opacity = 0.95;
    }),
    50
  )
});



//
// > Offline

window.addEventListener("online", onlineStatusChanged);
window.addEventListener("offline", onlineStatusChanged);

function onlineStatusChanged() {
  app.ports.setIsOnline.send(navigator.onLine);
}



//
// > Queue

app.ports.toggleRepeat.subscribe(bool => {
  audioEnvironmentContext.repeat = bool;
});



//
// > Search

const search = new Worker("/workers/search.js");

app.ports.performSearch.subscribe(searchTerm => {
  search.postMessage({
    action: "PERFORM_SEARCH",
    data: searchTerm
  });
});

app.ports.updateSearchIndex.subscribe(tracksJSON => {
  search.postMessage({
    action: "UPDATE_SEARCH_INDEX",
    data: tracksJSON
  });
});

search.onmessage = event => {
  switch (event.data.action) {
    case "PERFORM_SEARCH":
      app.ports.receiveSearchResults.send(event.data.data);
      break;
  }
};



//
// > Shortcuts

if (self.electron) {
  electron.ipcRenderer.send("register-shortcuts");

  // Next
  electron.ipcRenderer.on("media-next-track", _ => {
    app.ports.shortcutNext.send(null);
  });

  // Play & Pause
  electron.ipcRenderer.on("media-play-pause", _ => {
    app.ports.shortcutPlayPause.send(null);
  });

  // Previous
  electron.ipcRenderer.on("media-previous-track", _ => {
    app.ports.shortcutPrevious.send(null);
  });
}



//
// > Slave worker
//   (ie. the Elm worker)

const slave = new Worker("/workers/slave.js");

app.ports.slaveEvent.subscribe(aura => {
  slave.postMessage(aura);
});

slave.onmessage = event => {
  app.ports.slaveEventResult.send(event.data);
};



//
// > Sources

app.ports.requestLocalPath.subscribe(_ => {
  const lpath = electron.remote.dialog.showOpenDialog({
    properties: [ "openDirectory" ]
  });

  app.ports.receiveLocalPath.send((lpath && lpath[0]) || null);
});



//
// > Touch devices

window.addEventListener("touchstart", function onFirstTouch() {
  app.ports.setIsTouchDevice.send(true);
  window.removeEventListener("touchstart", onFirstTouch, false);
});
