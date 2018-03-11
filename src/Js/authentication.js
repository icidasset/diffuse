//
// Authentication
// [≡] 〆(・⺫・‶)
//
// Different integrations for authentication:
// 1. Local (indexedDB)
// 2. Blockstack
// 3. Remote Storage

const AUTH_SYSTEM = {};



//
// 🖍 Utensils

function construct(workerName) {
  return new Promise((resolve, reject) => {
    let worker;

    worker = new Worker(`/workers/authentication/${workerName}.js`);
    worker.onmessage = event => {
      worker.onmessage = null;

      switch (event.data.action) {
        case "CONSTRUCT_SUCCESS": return resolve(worker);
        case "CONSTRUCT_FAILURE": return reject();
      }
    };
  });
}


function doWork(worker, requisites) {
  const timeoutId = setTimeout(_ => {
    requisites.reject("Failed to reach web worker");
  }, 60000);

  // Wait for response from worker,
  // have timeout as fallback.
  const patchedHandler = event => {
    worker.removeEventListener("message", patchedHandler);
    clearTimeout(timeoutId);
    requisites.handler(event);
  };

  worker.addEventListener("message", patchedHandler);
  worker.postMessage({ action: requisites.action, data: requisites.data });
}


function prompt(question) {
  setTimeout(() => node.querySelector(".spinner").style.visibility = "hidden", 0);

  return x0p({
    inputPlaceholder: "example@5apps.com",
    maxWidth: "95vw",
    title: question,
    type: "input",
    width: "358px"
  }).then(data => {
    node.querySelector(".spinner").style.visibility = "visible";
    return data;
  });
}



//
// 📦 Caching

const CACHE = {
  isFetching: false
};

function afterCacheRetrieval(data) {
  CACHE.isFetching = false;

  app.ports.authenticationEventResult.send({
    tag: "GET_DATA",
    data: data,
    error: null
  });
}

function cache(json, cacheKey) {
  return AUTH_SYSTEM.LOCAL.storeData(json, cacheKey);
}

function cacheIsBusy() {
  return CACHE.isFetching;
}

function constructCache() {
  return AUTH_SYSTEM.LOCAL.construct();
}

function deconstructCache() {
  return AUTH_SYSTEM.LOCAL.deconstruct();
}

function retrieveCache(cacheKey) {
  CACHE.isFetching = true;

  return AUTH_SYSTEM.LOCAL.getData(cacheKey).then(cache => {
    if (!cache) { CACHE.isFetching = false; }
    return cache;
  });
}



//
// > Method

const METHOD_KEY =
  "authenticationMethod";


AUTH_SYSTEM.METHOD =
  { get:    _ => localStorage.getItem(METHOD_KEY)
  , set:    v => localStorage.setItem(METHOD_KEY, v)
  , unset:  _ => localStorage.removeItem(METHOD_KEY)
  };



//
// 1. Local

(() => {

  let ID = "authenticationMethod.LOCAL";
  let worker;


  AUTH_SYSTEM.LOCAL = {

    construct() {
      return construct("local").then(w => worker = w);
    },


    deconstruct() {
      worker.terminate();
      worker = null;

      return Promise.resolve();
    },


    // In & Out


    isSignedIn:
      () => Promise.resolve(!!localStorage.getItem(ID)),


    isSigningIn:
      () => Promise.resolve(false),


    handleSignInProcess:
      () => Promise.resolve("KeepUrl"),


    signIn() {
      localStorage.setItem(ID, "t");
      return Promise.resolve("None");
    },


    signOut() {
      localStorage.removeItem(ID);
      return Promise.resolve();
    },


    // Data


    getData: (cacheKey = null) => new Promise((resolve, reject) => {
      const handler = event => {
        switch (event.data.action) {
          case "GET_SUCCESS":   return event.data.data
                                    ? resolve( event.data.data )
                                    : resolve( null );

          case "GET_FAILURE":   return reject("Failed to get data");
          default:              return reject("Unavailable");
        }
      };

      doWork(worker, {
        action: "GET",
        data: { cacheKey: cacheKey },
        resolve: resolve,
        reject: reject,
        handler: handler
      });
    }),


    storeData: (json, cacheKey = null) => new Promise((resolve, reject) => {
      const handler = event => {
        switch (event.data.action) {
          case "SET_SUCCESS":   return resolve();
          case "SET_FAILURE":   return reject("Failed to store data");
          default:              return reject("Unavailable");
        }
      };

      doWork(worker, {
        action: "SET",
        data: { cacheKey: cacheKey, json: json },
        resolve: resolve,
        reject: reject,
        handler: handler
      });
    })

  }

})();



//
// 2. Blockstack

(() => {

  let KEY = "diffuse.json";
  let worker;


  AUTH_SYSTEM.BLOCKSTACK = {

    construct() {
      return construct("blockstack").then(w => worker = w);
    },


    deconstruct() {
      worker.terminate();
      worker = null;

      return Promise.resolve();
    },


    // In & Out


    isSignedIn:
      () => Promise.resolve(blockstack.isUserSignedIn()),


    isSigningIn:
      () => Promise.resolve(blockstack.isSignInPending()),


    handleSignInProcess:
      () => blockstack.handlePendingSignIn().then(_ => "ModifyUrl"),


    signIn() {
      blockstack.redirectToSignIn();
      return Promise.resolve("Redirect");
    },


    signOut() {
      blockstack.signUserOut();
      return Promise.resolve();
    },


    // Data


    getData: _ => blockstack.getFile(KEY),


    storeData: json => blockstack.putFile(KEY, json)

  }

})();



//
// 3. Remote Storage

(() => {

  let cacheKey = "remoteStorage";
  let rs;
  let worker;


  function setInstance() {
    if (rs) return;
    rs = new RemoteStorage({ cache: false });
    rs.access.claim("diffuse", "rw");
  }

  function destroyInstance() {
    if (!rs) return;
    rs.disconnect();
    rs = null;
  }


  AUTH_SYSTEM.REMOTE_STORAGE = {

    construct() {
      const key = "remote-storage";

      return construct(key)
        .then(w => worker = w)
        .then(_ => constructCache(key));
    },


    deconstruct() {
      worker.terminate();
      worker = null;

      deconstructCache();

      return Promise.resolve();
    },


    // In & Out


    isSignedIn: _ => new Promise((resolve, reject) => {
      let h;

      // RemoteStorage checks for the "access_token" param in the hash,
      // some other services that we use also use this.
      // So for the situations where RemoteStorage should not check
      // this param, make the hash empty and then after RemoteStorage
      // is set up, reset the hash to what it was before.
      if (location.pathname.startsWith("/sources/new/")) {
        h = location.hash;
        location.hash = "";
      }

      setInstance();

      const timeoutId = setTimeout(() => {
        rs.off("connected");
        if (h) location.hash = h;
        resolve(false);
      }, 10000);

      rs.on("connected", _ => {
        clearTimeout(timeoutId);
        rs.off("connected");
        if (h) location.hash = h;
        resolve(true);
      });
    }),


    isSigningIn:
      () => Promise.resolve(false),


    handleSignInProcess:
      () => Promise.resolve("KeepUrl"),


    signIn: _ => new Promise((resolve, reject) => {
      prompt(
        "What's your user address?"

      ).then(data => {
        if (data.button === "cancel" || data.text.length === 0) {
          return resolve("GoBack");
        }

        setInstance();

        rs.on("connected", _ => resolve("Redirect"));
        rs.on("error", err => reject(err.message));
        rs.connect(data.text);

      });
    }),


    signOut() {
      setInstance();
      destroyInstance();

      return Promise.resolve();
    },


    // Data


    getData: _ => new Promise((resolve, reject) => {
      const handler = callback => event => {
        switch (event.data.action) {
          case "GET_SUCCESS":   return event.data.data
                                    ? callback( event.data.data )
                                    : callback( null );

          case "GET_FAILURE":   return reject(`Failed to get data, ${event.data.data}.`);
          default:              return reject("Unavailable");
        }
      };

      retrieveCache(cacheKey).then(cacheData => {
        const handlerWithCallback = cacheData
          ? handler(afterCacheRetrieval)
          : handler(resolve);

        doWork(worker, {
          action: "GET",
          data: { token: rs.remote.token, userAddress: rs.remote.userAddress },
          resolve: resolve,
          reject: reject,
          handler: handlerWithCallback
        });

        if (cacheData) {
          resolve(cacheData);
        }
      });
    }),


    storeData: json => new Promise((resolve, reject) => {
      const handler = event => {
        switch (event.data.action) {
          case "SET_SUCCESS":   return resolve();
          case "SET_FAILURE":   return reject(`Failed to store data, ${event.data.data}.`);
          default:              return reject("Unavailable");
        }
      };

      if (cacheIsBusy()) {
        return resolve();
      }

      cache(json, cacheKey).then(_ => {
        doWork(worker, {
          action: "SET",
          data: { json: json, token: rs.remote.token, userAddress: rs.remote.userAddress },
          resolve: resolve,
          reject: reject,
          handler: handler
        });
      });
    })

  }

})();
