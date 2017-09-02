const _icidasset$ongaku_ryoho$Native_Authentication_Local = (() => {

  const KEY = "ongaku-ryoho_" + location.hostname + ".json";
  const ID = "authenticationMethod.LOCAL";

  const nativeBinding = _elm_lang$core$Native_Scheduler.nativeBinding;
  const succeed = _elm_lang$core$Native_Scheduler.succeed;
  const fail = _elm_lang$core$Native_Scheduler.fail;
  const Just = _elm_lang$core$Maybe$Just;
  const Nothing = _elm_lang$core$Maybe$Nothing;


  // indexedDB

  const indexedDB =
    window.indexedDB ||
    window.webkitIndexedDB ||
    window.mozIndexedDB ||
    window.msIndexedDB;

  let db;


  // Utils

  function native(fn) {
    return nativeBinding(callback => callback(succeed(fn())));
  }


  let worker;


  return {

    construct: nativeBinding(callback => {
      let idx;

      // worker
      worker = new Worker("/workers/authentication/local.js");

      // early return if already setup
      if (db) {
        callback(succeed());
        return;
      }

      idx = indexedDB.open(KEY, 1);
      idx.onupgradeneeded = event => {
        event.target.result.createObjectStore(KEY);
      };

      idx.onsuccess = () => {
        db = idx.result;
        callback(succeed());
      };

      idx.onerror = () => {
        callback(fail());
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


    // Data

    getData: nativeBinding(callback => {
      if (db) {
        const tra = db.transaction([KEY], "readwrite");
        const req = tra.objectStore(KEY).get(KEY);

        req.onsuccess = _ => {
          if (req.result) {
            const blob = req.result;
            const reader = new FileReader();

            reader.addEventListener("loadend", e => {
              callback(succeed(Just(e.srcElement.result)));
            });

            reader.readAsText(blob);

          } else {
            callback(succeed(Nothing));

          }
        };

        req.onerror = _ => {
          return callback(fail("Transaction error"));
        };

      } else {
        callback(fail("Unavailable"));

      }
    }),

    storeData(json) {
      return nativeBinding(callback => {
        if (db) {
          const blob = new Blob([json], { type: "application/json" });
          const tra = db.transaction([KEY], "readwrite");
          const req = tra.objectStore(KEY).put(blob, KEY);

          req.onsuccess = () => callback(succeed());
          req.onerror = () => callback(fail("Could not store data"));

        } else {
          callback(fail("Unavailable"));

        }
      });
    }

  };

})();
