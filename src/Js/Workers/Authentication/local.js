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
    case "GET": return get(event.data.data);
    case "SET": return set(event.data.data);
  }
};



//
// Get

function get(data) {
  const key = data.cacheKey ? data.cacheKey : KEY;
  const tra = db.transaction([KEY], "readwrite");
  const req = tra.objectStore(KEY).get(key);

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

function set(data) {
  const jso = data.json
  const key = data.cacheKey ? data.cacheKey : KEY;

  const buf = stringToArrayBuf(jso);
  const tra = db.transaction([KEY], "readwrite");
  const req = tra.objectStore(KEY).put(buf, key);

  req.onsuccess = () => self.postMessage({ action: "SET_SUCCESS" });
  req.onerror = () => self.postMessage({ action: "SET_FAILURE" });
}



//
// 🖍 Utensils

function arrayBufToString(buf) {
  return new encoding.TextDecoder("utf-8").decode(new Uint8Array(buf));
}


function stringToArrayBuf(str) {
  return new encoding.TextEncoder().encode(str).buffer;
}
