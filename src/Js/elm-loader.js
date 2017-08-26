const state = {
  auth: authenticationMethod(),
  dataContainer: {}
};


const node    = document.getElementById("elm-container");
const search  = new Worker("/search.js");


if (state.auth.isSignedIn()) {
  state.auth.getData()
    .then(keepDataInContainer)
    .then(initialize);

} else if (state.auth.isSigningIn()) {
  state.auth.handleSignInProcess();

} else {
  initialize({});

}



//
// Elm

function initialize(params) {
  node.innerHTML = "";

  const flags = initializeFlags(params);
  const app = Elm.App.embed(node, flags);

  initializePorts(app, flags);
  initializeSettings(flags);
  initializeTouchDetection(app);
}


function initializeFlags(params) {
  return {
    settings: {
      application: Object.assign(
        { backgroundImage: "4.jpg" },
        loadSettings("application")
      ),
      equalizer: Object.assign(
        { low: 0, mid: 0, high: 0, volume: 1 },
        loadSettings("equalizer")
      ),
      queue: Object.assign(
        { repeat: false, shuffle: false },
        loadSettings("queue")
      ),
      tracks: Object.assign(
        { favouritesOnly: false, searchTerm: null },
        loadSettings("tracks")
      )
    },

    user: state.auth.isSignedIn() ? state.auth.userData() : null,

    favourites: params.favourites || null,
    sources: params.sources || null,
    tracks: params.tracks || null
  };
}


function initializeSettings(flags) {
  const eq = flags.settings.equalizer;

  // > Equalizer

  low.gain.value      = determineNodeGainValue("Low", eq.low);
  mid.gain.value      = determineNodeGainValue("Mid", eq.mid);
  high.gain.value     = determineNodeGainValue("High", eq.high);
  volume.gain.value   = determineNodeGainValue("Volume", eq.volume);
}


function initializePorts(app, flags) {
  // > Authentication

  app.ports.authenticate.subscribe(method => {
    state.auth = setAuthenticationMethod(method);
    state.auth.signIn();
  });

  app.ports.deauthenticate.subscribe(() => {
    state.auth.signOut();
    unsetAuthenticationMethod();
  });

  // > Audio

  const audioEnvironmentContext = {
    activeQueueItem: null,
    elm: app
  };

  app.ports.activeQueueItemChanged.subscribe(item => {
    const timestampInMilliseconds = Date.now();

    audioEnvironmentContext.activeQueueItem = item;
    audioEnvironmentContext.audio = null;

    if (item) {
      removeOlderAudioElements(timestampInMilliseconds);
      insertTrack(audioEnvironmentContext, item);
    } else {
      removeOlderAudioElements(timestampInMilliseconds);
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

  // > Equalizer

  app.ports.adjustEqualizerSetting.subscribe(e => {
    let node;

    switch (e.knob) {
      case "Low"      : node = low; break;
      case "Mid"      : node = mid; break;
      case "High"     : node = high; break;
      case "Volume"   : node = volume; break;
    }

    node.gain.value = determineNodeGainValue(e.knob, e.value);
  });

  // > Processing

  app.ports.requestTags.subscribe(distantContext => {
    const context = Object.assign({}, distantContext);
    const initialPromise = Promise.resolve([]);

    return context.urlsForTags.reduce((accumulator, urls) => {
      return accumulator.then(col =>
        getTags(urls.getUrl, urls.headUrl)
          .then(r => col.concat(r))
          .catch(e => {
            console.error(e);
            return col.concat(null);
          })
      );

    }, initialPromise).then(col => {
      context.receivedTags = _.compose(
        x => _.map(pickTags, x),
        x => _.filter(_.isObject, x)
      )(
        col
      );

      app.ports.receiveTags.send(context);

    });
  });

  // > Data

  app.ports.storeSources.subscribe(v => storeData("sources", v));
  app.ports.storeTracks.subscribe(v => storeData("tracks", v));
  app.ports.storeFavourites.subscribe(v => storeData("favourites", v));

  app.ports.storeApplicationSettings.subscribe(s => saveSettings("application", s));
  app.ports.storeEqualizerSettings.subscribe(s => saveSettings("equalizer", s));
  app.ports.storeQueueSettings.subscribe(s => saveSettings("queue", s));
  app.ports.storeTracksSettings.subscribe(s => saveSettings("tracks", s));

  // > Search

  app.ports.performSearch.subscribe(searchTerm => {
    search.postMessage({
      action: "perform_search",
      data: searchTerm
    });
  });

  app.ports.updateSearchIndex.subscribe(tracksJSON => {
    search.postMessage({
      action: "update_search_index",
      data: tracksJSON
    });
  });

  search.onmessage = event => {
    switch (event.data.action) {
      case "perform_search":
        app.ports.receiveSearchResults.send(event.data.data);
        break;
    }
  };

  search.postMessage({
    action: "update_search_index",
    data: flags.tracks
  });
}


function initializeTouchDetection(app) {
  window.addEventListener("touchstart", function onFirstTouch() {
    app.ports.setIsTouchDevice.send(true);
    window.removeEventListener('touchstart', onFirstTouch, false);
  });
}



//
// Data

function storeData(key, data) {
  state.dataContainer = _.put(key, data, state.dataContainer);
  return state.auth.storeData(state.dataContainer);
}


function keepDataInContainer(data) {
  state.dataContainer = data;
  return data;
}



//
// Settings

function saveSettings(key, settings) {
  localStorage.setItem("settings." + key, JSON.stringify(settings));
}


function loadSettings(key) {
  const val = localStorage.getItem("settings." + key);
  if (!val || !val.length) return null;

  try {
    return JSON.parse(val);
  } catch (_) {
    return {};
  }
}
