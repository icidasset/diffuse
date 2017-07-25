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
    this.schema = {
      favourites: 'artist,title',
      sources: 'id,data,enabled,service',
      tracks: 'id,path,sourceId,tags'
    };

    this.db = new Dexie(VERSION_KEY);
    this.db.version(1).stores(this.schema);

    return Promise.all([
      this.db.favourites.toArray(),
      this.db.sources.toArray(),

      // load tracks in batches
      this.db.tracks.count()
        .then(n => new DexieBatch({ batchSize: 1000, limit: n }))
        .then(b => {
          let tracks = [];
          let tracksCol = this.db.tracks.toCollection();

          return b.eachBatch(tracksCol, batch => tracks = tracks.concat(batch))
                  .then(_ => tracks);
        })

    ]).then(x => {
      return { favourites: x[0], sources: x[1], tracks: x[2] };

    });
  },

  storeData(data) {
    const errHandler = err => console.error(err);

    return Object.keys(data).map(
      key => {
        const items = (data[key] || []).slice();
        return _ => this.db[key].bulkPut(items);
      }
    ).reduce(
      (acc, fn) => acc.then(fn).catch(errHandler),
      Promise.resolve()
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
