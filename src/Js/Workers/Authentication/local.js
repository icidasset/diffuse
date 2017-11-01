importScripts("/vendor/package.js");


const KEY = location.hostname + ".json";


//
// Construct

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
      self.postMessage({ action: "GET_SUCCESS", data: arrayBufToString(req.result) });
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
  const buf = stringToArrayBuf(json);
  const tra = db.transaction([KEY], "readwrite");
  const req = tra.objectStore(KEY).put(buf, KEY);

  req.onsuccess = () => self.postMessage({ action: "SET_SUCCESS" });
  req.onerror = () => self.postMessage({ action: "SET_FAILURE" });
}



//
// 🖍 Utensils

function arrayBufToString(buf) {
  return String.fromCharCode.apply(null, new Uint16Array(buf));
}


function stringToArrayBuf(str) {
  const buf = new ArrayBuffer(str.length * 2);
  const bufView = new Uint16Array(buf);

  for (let i = 0; i < str.length; i++) {
    bufView[i] = str.charCodeAt(i);
  }

  return buf;
}
