var node = document.getElementById("elm-container");
var userProps = ["displayName", "email", "photoURL", "refreshToken", "uid"];
var didSetupElm = false;

//
// Firebase

var firebaseConfig = {
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
  var maybeUser = userObj ? _.pick(userProps, userObj) : null;
  setupElm({ user: maybeUser });
}



//
// Elm

function setupElm(params) {
  didSetupElm = true;

  // embed
  var app = Elm.App.embed(
    node,
    { user: params.user || null }
  );

  // ports
  app.ports.authenticate.subscribe(function() {
    firebase.auth().signInWithRedirect(
      new firebase.auth.GoogleAuthProvider()
    );
  });
}
