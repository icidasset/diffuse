let node = document.getElementById("elm-container");
let userProps = ["displayName", "email", "photoURL", "refreshToken", "uid"];
let didSetupElm = false;



//
// Firebase

const firebaseConfig = {
  apiKey: "AIzaSyCEblV1BTVwpMCdAWZlchV23qJCsW_PCjw",
  authDomain: "ongaku-ryoho-v3-test.firebaseapp.com",
  databaseURL: "https://ongaku-ryoho-v3-test.firebaseio.com"
};

firebase.initializeApp(firebaseConfig);
firebase.auth().getRedirectResult(); // TODO - Handle errors
firebase.auth().onAuthStateChanged(authStatechange); // TODO - Handle errors


function authStatechange(userObj) {
  if (didSetupElm) return;

  // go ahead
  const maybeUser = userObj ? _.pick(userProps, userObj) : null;
  setupElm({ user: maybeUser });
}



//
// Elm

function setupElm(params) {
  didSetupElm = true;

  // Clean
  node.innerHTML = "";

  // Embed
  const app = Elm.App.embed(
    node,
    { settings:
        { queue: { repeat: false, shuffle: false } // TODO
        }
    , user: params.user || null
    }
  );

  // Ports
  // > Authentication
  app.ports.authenticate.subscribe(() => {
    firebase.auth().signInWithRedirect(
      new firebase.auth.GoogleAuthProvider()
    );
  });

  // > Audio
  var audioEnvironmentContext = {
    activeQueueItem: null,
    elm: app
  };

  app.ports.activeQueueItemChanged.subscribe((item) => {
    const timestampInMilliseconds = Date.now();

    audioEnvironmentContext.activeQueueItem = item;

    createAudioElement(audioEnvironmentContext, item);
    removeOlderAudioElements(timestampInMilliseconds);
  });
}
