//
// Authentication
// [≡] 〆(・⺫・‶)
//
// Different integrations for authentication:
// 1. Local (indexedDB)
// 2. Blockstack

const AUTH_SYSTEM = {};



//
// > Utilities

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


function promised(fn) {
  return () => new Promise(fn);
}


function resolved(fn) {
  return () => Promise.resolve(fn.call(this));
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

    construct: promised((resolve, reject) => {
      worker = new Worker("/workers/authentication/local.js");
      worker.onmessage = event => {
        worker.onmessage = null;

        switch (event.data.action) {
          case "CONSTRUCT_SUCCESS": return resolve();
          case "CONSTRUCT_FAILURE": return reject();
        }
      };
    }),

    deconstruct: resolved(() => {
      worker.terminate();
      worker = null;
    }),

    // In & Out

    isSignedIn: resolved(
      () => !!localStorage.getItem(ID)
    ),

    isSigningIn: resolved(
      () => false
    ),

    handleSignInProcess: resolved(() => {
      return "KeepUrl";
    }),

    signIn: resolved(() => {
      localStorage.setItem(ID, "t");
      return "None";
    }),

    signOut: resolved(() => {
      localStorage.removeItem(ID);
    }),

    // Get data

    getData: promised((resolve, reject) => {
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
        data: null,
        resolve: resolve,
        reject: reject,
        handler: handler
      });
    }),

    // Store data

    storeData: json => new Promise((resolve, reject) => {
      const handler = event => {
        switch (event.data.action) {
          case "SET_SUCCESS":   return resolve();
          case "SET_FAILURE":   return reject("Failed to store data");
          default:              return reject("Unavailable");
        }
      };

      doWork(worker, {
        action: "SET",
        data: json,
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

  let KEY = "isotach.json";
  let worker;


  AUTH_SYSTEM.BLOCKSTACK = {

    construct: resolved(() => {
      worker = new Worker("/workers/authentication/blockstack.js");
    }),

    deconstruct: resolved(() => {
      worker.terminate();
      worker = null;
    }),

    // In & Out

    isSignedIn: resolved(
      blockstack.isUserSignedIn
    ),

    isSigningIn: resolved(
      blockstack.isSignInPending
    ),

    handleSignInProcess: promised((resolve, reject) => {
      blockstack.handlePendingSignIn().then(
        user  => resolve("ModifyUrl"),
        err   => reject(err)
      );
    }),

    signIn: resolved(() => {
      blockstack.redirectToSignIn();
      return "Redirect";
    }),

    signOut: resolved(() => {
      blockstack.signUserOut();
    }),

    // Get data

    getData: promised((resolve, reject) => {
      blockstack.getFile(KEY).then(
        resolve,
        reject
      );
    }),

    // Store data

    storeData: json => new Promise((resolve, reject) => {
      blockstack.putFile(KEY, json).then(
        _    => resolve(),
        err  => reject(err)
      );
    })

  }

})();
