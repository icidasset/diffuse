/**
 * Different authentication methods.
 *
 * 1. Local
 * 2. Blockstack
 *
 */


const VERSION_KEY = "ongaku-ryoho-v1_0.json";
const METHOD_KEY  = "authMethod";
const METHODS     = { LOCAL: "LOCAL", BLOCKSTACK: "BLOCKSTACK" };
const M           = {};


function authenticationMethod() {
  const method = localStorage.getItem(METHOD_KEY) || "FALLBACK";
  return M[method];
}


function setAuthenticationMethod(method) {
  localStorage.setItem(METHOD_KEY, method);
  return M[method];
}


function unsetAuthenticationMethod() {
  localStorage.removeItem(METHOD_KEY);
}



// 0. FALLBACK (NO METHOD SELECTED)
//
M.FALLBACK = {

  isSignedIn() {},
  isSigningIn() {},
  handleSignInProcess() {},
  signIn() {},
  signOut() {},
  getData() {},
  storeData(data) {},
  userData() {}

};



// 1. LOCAL
//

M.LOCAL = {

  isSignedIn() {
    return localStorage.getItem("signedInAnonymously") === "t";
  },

  isSigningIn() {
    return false;
  },

  handleSignInProcess() {
    return false;
  },

  signIn() {
    localStorage.setItem("signedInAnonymously", "t");
    window.location.reload();
  },

  signOut() {
    localStorage.removeItem("signedInAnonymously");
  },

  // Data

  getData() {
    return Promise.resolve(
      JSON.parse(localStorage.getItem(VERSION_KEY) || "{}")
    );
  },

  storeData(data) {
    return Promise.resolve(
      localStorage.setItem(VERSION_KEY, JSON.stringify(data))
    );
  },

  userData() {
    return { displayName: "anonymous" };
  }

};



// 2. BLOCKSTACK
//

M.BLOCKSTACK = {

  isSignedIn() {
    return blockstack.isUserSignedIn();
  },

  isSigningIn() {
    return blockstack.isSignInPending();
  },

  handleSignInProcess() {
    return blockstack.handlePendingSignIn().then(
      _   => {
        setAuthenticationMethod(METHODS.BLOCKSTACK);
        window.location = window.location.origin;
      },
      err => {
        console.error("Failed to authenticate", err);
      }
    );
  },

  signIn() {
    blockstack.redirectToSignIn();
  },

  signOut() {
    blockstack.signUserOut();
  },

  // Data

  getData() {
    return blockstack
      .getFile(VERSION_KEY)
      .then(data => JSON.parse(data || "{}"));
  },

  storeData(data) {
    return blockstack.putFile(
      VERSION_KEY,
      JSON.stringify(data)
    );
  },

  userData() {
    const userData = blockstack.loadUserData();
    const name = userData.username || "anonymous";

    return { displayName: name };
  }

};
