let node = document.getElementById("elm-container");
let dataContainer = {};



//
// Search

const search = new Worker("search.js");



//
// Blockstack

const BLOCKSTACK_FILE_PATH = "ongaku-ryoho-v1_0.json";


// {state} Authenticated
if (blockstack.isUserSignedIn()) {
  const userData = blockstack.loadUserData();
  const name = userData.username || "anonymous";

  getData()
    .then(d => _.assign(d, { user: { displayName: name }}))
    .then(setupElm)
    .catch(_ => console.error("Failed to load application data"));

// {state} Is authenticating
} else if (blockstack.isSignInPending()) {
  blockstack.handlePendingSignIn().then(
    _   => window.location = window.location.origin,
    err => console.error("Failed to authenticate", err)
  );

// {state} Not authenticated
} else {
  setupElm({ user: null });

}


/**
 * Get the application data from the Blockstack storage.
 */
function getData() {
  return blockstackStorage.getFile(BLOCKSTACK_FILE_PATH)
    .then(con => dataContainer = JSON.parse(con || "{}"))
    .then(_   => dataContainer);
}


/**
 * Store some data in the Blockstack storage.
 */
function storeData(key, data) {
  dataContainer = _.set(key, data, dataContainer);

  // Store
  const content = JSON.stringify(dataContainer);
  return blockstackStorage.putFile(BLOCKSTACK_FILE_PATH, content);
}



//
// Settings

function saveSettings(key, settings) {
  localStorage.setItem("settings." + key, JSON.stringify(settings));
}


function loadSettings(key) {
  let val = localStorage.getItem("settings." + key);
  if (!val || !val.length) return null;

  try {
    return JSON.parse(val);
  } catch (_) {
    return {};
  }
}



//
// Elm

function setupElm(params) {
  node.innerHTML = "";

  // Flags
  const flags = {
    settings: {
      queue: Object.assign(
        { repeat: false, shuffle: false },
        loadSettings("queue")
      ),
      tracks: Object.assign(
        { favouritesOnly: false, searchTerm: null },
        loadSettings("tracks")
      )
    },

    user: params.user,

    favourites: params.favourites || null,
    sources: params.sources || null,
    tracks: params.tracks || null
  };

  // Embed
  const app = Elm.App.embed(node, flags);

  // Ports
  // > Authentication

  app.ports.authenticate.subscribe(() => {
    blockstack.redirectToSignIn();
  });

  app.ports.deauthenticate.subscribe(() => {
    blockstack.signUserOut(window.location.origin);
  });

  // > Audio

  var audioEnvironmentContext = {
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
      app.ports.setProgress.send(0);
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

  // > Processing

  app.ports.requestTags.subscribe(distantContext => {
    const context = Object.assign({}, distantContext);
    const initialPromise = Promise.resolve([]);

    return context.urlsForTags.reduce((accumulator, urls) => {
      return accumulator.then(
        col => {
          return getTags(urls.getUrl, urls.headUrl)
            .then(r => col.concat(r))
            .catch(e => {
              console.error(e);
              return col.concat(null);
            });
        }
      );

    }, initialPromise).then(col => {
      context.receivedTags = _.compact(col).map(pickTags);
      app.ports.receiveTags.send(context);

    });
  });

  // > Data

  app.ports.storeSources.subscribe(v => storeData("sources", v));
  app.ports.storeTracks.subscribe(v => storeData("tracks", v));
  app.ports.storeFavourites.subscribe(v => storeData("favourites", v));

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
