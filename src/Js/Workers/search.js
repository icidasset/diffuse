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
  album: track.tags.album,
  artist: track.tags.artist,
  title: track.tags.title
});



//
// Actions

const FIELDS = {
  album: { boost: 1 },
  artist: { boost: 3, bool: "AND" },
  title: { boost: 2 }
};


function performSearch(searchTerm) {
  const results = index
    .search(searchTerm, { bool: "OR", expand: true, fields: FIELDS })
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

  index = elasticlunr(function() {
    const i = this;

    i.setRef("id");

    i.addField("album");
    i.addField("artist");
    i.addField("title");

    (tracks || [])
      .map(mapTrack)
      .forEach(t => i.addDoc(t));
  });
}
