importScripts("/vendor/package.js");


const KEY = location.hostname + ".json";


//
// Setup DB

const indexedDB =
  self.indexedDB ||
  self.webkitIndexedDB ||
  self.mozIndexedDB ||
  self.msIndexedDB;


let db, idx;


idx = indexedDB.open(KEY, 1);
idx.onupgradeneeded = event => {
  event.target.result.createObjectStore(KEY);
}

idx.onsuccess = _ => {
  db = idx.result;
  self.postMessage({ action: "CONSTRUCT_SUCCESS" });
};

idx.onerror = _ => {
  self.postMessage({ action: "CONSTRUCT_FAILURE" });
};



//
// Incoming messages

self.onmessage = event => {
  if (!db) return self.postMessage({ action: "NO_DB" });

  switch (event.data.action) {
    case "GET": return get();
    case "SET": return set(event.data.data);
  }
};



//
// Get

function get() {
  const tra = db.transaction([KEY], "readwrite");
  const req = tra.objectStore(KEY).get(KEY);

  req.onsuccess = _ => {
    if (req.result) {
      const blob = req.result;
      const reader = new FileReader();

      reader.addEventListener("loadend", e => {
        self.postMessage({ action: "GET_SUCCESS", data: e.srcElement.result });
      });

      reader.readAsText(blob);

    } else {
      self.postMessage({ action: "GET_SUCCESS", data: null });

    }
  };

  req.onerror = _ => {
    self.postMessage({ action: "GET_FAILURE" });
  };
}



//
// Set

function set(json) {
  const blob = new Blob([json], { type: "application/json" });
  const tra = db.transaction([KEY], "readwrite");
  const req = tra.objectStore(KEY).put(blob, KEY);

  req.onsuccess = () => self.postMessage({ action: "SET_SUCCESS" });
  req.onerror = () => self.postMessage({ action: "SET_FAILURE" });
}
