importScripts("/vendor/package.js");


const KEY       = "isotach";


//
// Construct

const rs = new RemoteStorage({ cache: false });
const client = rs.scope(`/${KEY}/`);

rs.access.claim(KEY, "rw");
self.postMessage({ action: "CONSTRUCT_SUCCESS" });



//
// Incoming messages

self.onmessage = event => {
  if (!rs.remote.connected) {
    rs.on("connected", () => tackle(event));
    rs.connect(event.data.data.userAddress, event.data.data.token);
  } else {
    tackle(event);
  }
};


function tackle(event) {
  switch (event.data.action) {
    case "GET": return get();
    case "SET": return set(event.data.data.json);
  }
}



//
// Get

function get() {
  client.getFile(`${KEY}.json`).then(
    res => self.postMessage({ action: "GET_SUCCESS", data: res.data }),
    err => self.postMessage({ action: "GET_SUCCESS", data: null })
  );
}



//
// Set

function set(json) {
  client.storeFile("application/json", `${KEY}.json`, json).then(
    res => self.postMessage({ action: "SET_SUCCESS" }),
    err => self.postMessage({ action: "SET_FAILURE", data: err })
  );
}
