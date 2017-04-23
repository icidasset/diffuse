importScripts("vendor/search.js");


let index;



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

function performSearch(searchTerm) {
  const results = index
    .search(searchTerm, { bool: "AND" })
    .map(s => s.ref);

  self.postMessage({
    action: "perform_search",
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



//
// Incoming messages

self.onmessage = event => {
  switch (event.data.action) {
    case "perform_search":
      performSearch(event.data.data);
      break;

    case "update_search_index":
      updateSearchIndex(event.data.data);
      break;
  }
};
