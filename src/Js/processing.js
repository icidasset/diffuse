//
// Tag reader

function getTags(urlGET, urlHEAD) {
  const fakeURL = "THIS_WONT_BE_USED_ANYWAYS";

  const reader = new jsmediatags.Reader(fakeURL);
  const fileReader = new XhrFileReader(fakeURL);
  const makeXHRRequest = fileReader._makeXHRRequest;

  fileReader._createXHRObject = function() {
    return new XMLHttpRequest();
  };

  fileReader._makeXHRRequest = function(method, ...args) {
    this._url = method.toUpperCase() === "HEAD" ? urlHEAD : urlGET;
    return makeXHRRequest.call(this, method, ...args);
  };

  return new Promise((resolve, reject) => {
    fileReader.init({
      onSuccess: () => {

        reader._getTagReader(fileReader, {
          onSuccess: (TagReader) => {
            new TagReader(fileReader)
              .setTagsToRead(reader._tagsToRead)
              .read({ onSuccess: resolve, onError: reject });
          },
          onError: reject
        });

      },
      onError: reject,
    });
  });
}


//
// Get the tags we need

function pickTags(tagsFromJsmediatags) {
  const tags = _.pick(
    ["album", "artist", "disk", "genre", "title", "track", "year"],
    tagsFromJsmediatags.tags
  );

  return {
    disc: (tags.disk ? parseInt(tags.disk, 10) : 1) || 1,
    nr: (tags.track ? parseInt(tags.track, 10) : 1) || 1,
    album: tags.album && tags.album.length ? tags.album : "Unknown",
    artist: tags.artist && tags.artist.length ? tags.artist : "Unknown",
    title: tags.title && tags.title.length ? tags.title : "Unknown",
    genre: tags.genre && tags.genre.length ? tags.genre : null,
    year: tags.year && tags.year.length ? getYear(tags.year) : null,
  };
}


//
// Utils

function getYear(dateStr) {
  return (new Date(dateStr)).getFullYear();
}
