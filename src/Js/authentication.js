/**
 * Different authentication methods.
 *
 * 1. Local
 * 2. Blockstack
 * 3. IPNS/IPFS
 *
 */


const VERSION_KEY = "ongaku-ryoho-v1_0_1.json";
const METHOD_KEY  = "authMethod";
const METHODS     = { LOCAL: "LOCAL", BLOCKSTACK: "BLOCKSTACK", IPNS: "IPNS" };
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
    location.reload();
  },

  signOut() {
    localStorage.removeItem("signedInAnonymously");
  },

  // Data

  getData() {
    this.db = new Dexie(VERSION_KEY);
    this.db.version(1).stores({
      favourites: '++, artist, title',
      sources: 'id, data, enabled, service',
      tracks: 'id, path, sourceId, tags'
    });

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

    Promise.all([
      this.db.favourites.clear(),
      this.db.sources.clear(),
      this.db.tracks.clear()

    ]).then(_ => {
      return Object.keys(data).map(
        key => {
          const items = (data[key] || []).slice();
          return _ => this.db[key].bulkPut(items);
        }
      ).reduce(
        (acc, fn) => acc.then(fn).catch(errHandler),
        Promise.resolve()
      );

    });
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



// 3. IPNS
//

M.IPNS = {

  isSignedIn() {
    return !!localStorage.getItem("authMethod.IPNS.passphrase");
  },

  isSigningIn() {
    return false;
  },

  handleSignInProcess() {},

  signIn() {
    const apiAddress = "http://localhost:5001";
    const gatewayAddress = "http://localhost:8080";
    const passphrase = prompt(
      "Which passphrase would you like to use? " +
      "Keep this somewhere safe, this is used to encrypt your data."
    );

    localStorage.setItem("authMethod.IPNS.api", apiAddress);
    localStorage.setItem("authMethod.IPNS.gateway", gatewayAddress);
    localStorage.setItem("authMethod.IPNS.passphrase", passphrase);

    const publicKey = encodeURIComponent(VERSION_KEY);

    // Ensure a IPNS key pair
    fetch(apiAddress + "/api/v0/key/list")
      .then(response => response.json())
      .then(response => {
        const pk = response.Keys.find(k => k.Name == publicKey);
        const privateKey = pk && pk.Id;

        // key pair already exists
        if (privateKey) {
          localStorage.setItem("authMethod.IPNS.privateKey", privateKey);
          location.reload();

        // create key pair + store empty data-set
        } else {
          fetch(apiAddress + "/api/v0/key/gen?arg=" + publicKey + "&type=rsa&size=2048")
            .then(response => response.json())
            .then(response => {
              localStorage.setItem("authMethod.IPNS.privateKey", response.Id);
              this.storeData({}).then(_ => location.reload());
            });

        }
      })
      .catch(this.error);
  },

  signOut() {
    localStorage.removeItem("authMethod.IPNS.api");
    localStorage.removeItem("authMethod.IPNS.gateway");
    localStorage.removeItem("authMethod.IPNS.passphrase");
  },

  // Data

  getData() {
    const apiAddress = localStorage.getItem("authMethod.IPNS.api");
    const gatewayAddress = localStorage.getItem("authMethod.IPNS.gateway");
    const privateKey = localStorage.getItem("authMethod.IPNS.privateKey");

    // 1. Resolve the IPNS item
    // 2. Get the data via IPFS
    // 3. Decrypt and parse
    return fetch(apiAddress + "/api/v0/name/resolve?resolve=false&arg=" + privateKey)
      .then(response => response.json())
      .then(response => fetch(gatewayAddress + response.Path))
      .then(response => response.text())
      .then(response => this.decrypt(response))
      .then(response => {
        try {
          return JSON.parse(response);
        } catch (err) {
          alert(
            "It seems that you previously signed in with a different passphrase, " +
            "please use that passphrase to sign in."
          );

          unsetAuthenticationMethod();
          location.reload();
        }
      })
      .catch(this.error);
  },

  storeData(data) {
    const apiAddress = localStorage.getItem("authMethod.IPNS.api");

    // Data -> JSON blob
    const encryptedData = this.encrypt(JSON.stringify(data));
    const encryptedBlob = new Blob([encryptedData], { type: "text/plain" });

    const formData = new FormData();
    formData.append("json", new File([encryptedBlob], VERSION_KEY));

    // 1. Add the blob to IPFS
    // 2. Publish the hash to the IPNS item
    return fetch(apiAddress + "/api/v0/add", { method: "POST", body: formData })
      .then(response => response.json())
      .then(response => fetch(
        apiAddress + "/api/v0/name/publish" +
          "?arg=" + response.Hash +
          "&key=" + encodeURIComponent(VERSION_KEY)
      ))
      .catch(this.error);
  },

  userData() {
    return { displayName: "ipns" };
  },

  // Utils

  encrypt(json) {
    return _.composeAll(
      [ x => aes.utils.hex.fromBytes(x)
      , i => i.encrypt(aes.utils.utf8.toBytes(json))
      , k => new aes.ModeOfOperation.ctr(k)
      , x => aes.utils.utf8.toBytes(x)
      , x => x.slice(0, 32)
      , x => sha256.create().update(VERSION_KEY + x).hex()
      , x => localStorage.getItem(x)
      ]
    )(
      "authMethod.IPNS.passphrase"
    );
  },

  decrypt(text) {
    return _.composeAll(
      [ x => aes.utils.utf8.fromBytes(x)
      , i => i.decrypt(aes.utils.hex.toBytes(text))
      , k => new aes.ModeOfOperation.ctr(k)
      , x => aes.utils.utf8.toBytes(x)
      , x => x.slice(0, 32)
      , x => sha256.create().update(VERSION_KEY + x).hex()
      , x => localStorage.getItem(x)
      ]
    )(
      "authMethod.IPNS.passphrase"
    );
  },

  error(err) {
    if (err.response) {
      console.error(err);
      alert("Something seems to have gone wrong. Please create an issue on `github.com/icidasset/ongaku-ryoho`. You can find more info in your browser console.")
    } else {
      alert("Couldn't reach your local IPFS API server, are you sure it's running?");
    }

    M.IPNS.signOut();
    unsetAuthenticationMethod();
    location.reload();

    return Promise.reject(err);
  }

};
