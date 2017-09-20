const _icidasset$ongaku_ryoho$Native_Authentication_Local = (() => {

  const ID = "authenticationMethod.LOCAL";

  const nativeBinding = _elm_lang$core$Native_Scheduler.nativeBinding;
  const succeed = _elm_lang$core$Native_Scheduler.succeed;
  const fail = _elm_lang$core$Native_Scheduler.fail;
  const Just = _elm_lang$core$Maybe$Just;
  const Nothing = _elm_lang$core$Maybe$Nothing;


  // Utils

  function native(fn) {
    return nativeBinding(callback => callback(succeed(fn())));
  }


  function doWork(reqs) {
    const timeoutId = setTimeout(_ => {
      reqs.callback(fail("Failed to reach web worker"));
    }, 60000);

    // Wait for response from worker,
    // have timeout as fallback.
    const patchedHandler = event => {
      worker.removeEventListener("message", patchedHandler);
      clearTimeout(timeoutId);
      reqs.handler(event);
    };

    worker.addEventListener("message", patchedHandler);
    worker.postMessage({ action: reqs.action, data: reqs.data });
  }


  let worker;


  return {

    construct: nativeBinding(callback => {
      worker = new Worker("/workers/authentication/local.js");
      worker.onmessage = event => {
        worker.onmessage = null;

        switch (event.data.action) {
          case "CONSTRUCT_SUCCESS": return callback(succeed());
          case "CONSTRUCT_FAILURE": return callback(fail());
        }
      };
    }),

    deconstruct: native(() => {
      worker.terminate();
      worker = null;
    }),

    // In & Out

    isSignedIn: native(
      () => !!localStorage.getItem(ID)
    ),

    isSigningIn: native(
      () => false
    ),

    handleSignInProcess: native(
      () => ({ ctor: "KeepUrl" })
    ),

    signIn: native(() => {
      localStorage.setItem(ID, "t");
      return { ctor: "None" };
    }),

    signOut: native(() => {
      localStorage.removeItem(ID)
    }),


    // GET DATA

    getData: nativeBinding(callback => {
      const handler = event => {
        switch (event.data.action) {
          case "GET_SUCCESS":   return event.data.data
                                    ? callback(succeed( Just(event.data.data) ))
                                    : callback(succeed( Nothing ));

          case "GET_FAILURE":   return callback(fail("Failed to get data"));
          default:              return callback(fail("Unavailable"));
        }
      };

      doWork({
        action: "GET",
        data: null,
        callback: callback,
        handler: handler
      });
    }),

    // STORE DATA

    storeData(json) { return nativeBinding(callback => {
      const handler = event => {
        switch (event.data.action) {
          case "SET_SUCCESS":   return callback(succeed());
          case "SET_FAILURE":   return callback(fail("Failed to store data"));
          default:              return callback(fail("Unavailable"));
        }
      };

      doWork({
        action: "SET",
        data: json,
        callback: callback,
        handler: handler
      });
    })}

  };

})();
