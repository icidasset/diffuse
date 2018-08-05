//
// Search worker
// (◡ ‿ ◡ ✿)
//
// This worker is responsible for searching through a `Track` collection.

importScripts("/vendor/package.js");


let index;


//
// Incoming messages

self.onmessage = event => {
  switch (event.data.action) {
    case "PERFORM_SEARCH":
      performSearch(event.data.data);
      break;

    case "UPDATE_SEARCH_INDEX":
      updateSearchIndex(event.data.data);
      break;
  }
};



//
// Track -> IndexedTrack

const mapTrack = track => ({
  id: track.id,
  album: [track.tags.album],
  artist: [track.tags.artist],
  title: [track.tags.title]
});



//
// Actions

function performSearch(searchTerm) {
  const properSearchTerm = searchTerm
    .replace(" *", "");

  const results = index
    .search(properSearchTerm)
    .map(s => s.ref);

  self.postMessage({
    action: "PERFORM_SEARCH",
    data: results
  });
}


function updateSearchIndex(input) {
  const tracks = typeof input == "string"
    ? JSON.parse(input)
    : input;

  index = lunr(function() {
    this.field("album");
    this.field("artist");
    this.field("title");

    (tracks || [])
      .map(mapTrack)
      .forEach(t => this.add(t));
  });
}
