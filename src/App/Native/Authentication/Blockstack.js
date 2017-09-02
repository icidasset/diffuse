const _icidasset$ongaku_ryoho$Native_Authentication_Blockstack = (() => {

  const KEY = "ongaku-ryoho.json";

  const nativeBinding = _elm_lang$core$Native_Scheduler.nativeBinding;
  const succeed = _elm_lang$core$Native_Scheduler.succeed;
  const fail = _elm_lang$core$Native_Scheduler.fail;
  const Just = _elm_lang$core$Maybe$Just;
  const Nothing = _elm_lang$core$Maybe$Nothing;


  function native(fn) {
    return nativeBinding(callback => callback(succeed(fn())));
  }


  let worker;


  return {

    construct: native(() => {
      worker = new Worker("/workers/authentication/blockstack.js");
    }),

    deconstruct: native(() => {
      worker.terminate();
      worker = null;
    }),

    // In & Out

    isSignedIn: native(
      blockstack.isUserSignedIn
    ),

    isSigningIn: native(
      blockstack.isSignInPending
    ),

    handleSignInProcess: nativeBinding(callback => {
      blockstack.handlePendingSignIn().then(
        user  => callback(succeed({ ctor: "ModifyUrl" })),
        err   => callback(fail(err))
      );
    }),

    signIn: native(() => {
      blockstack.redirectToSignIn();
      return { ctor: "Redirect" };
    }),

    signOut: native(
      blockstack.signUserOut
    ),


    // Data

    getData: nativeBinding(callback => {
      blockstack.getFile(KEY).then(
        data => callback(succeed(data ? Just(data) : Nothing)),
        err  => callback(fail(err))
      );
    }),

    storeData(json) {
      return nativeBinding(callback => {
        blockstack.putFile(KEY, json).then(
          _    => callback(succeed()),
          err  => callback(fail(err))
        );
      });
    }

  };

})();
