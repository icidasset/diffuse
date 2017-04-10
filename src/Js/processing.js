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
    ["album", "artist", "genre", "title", "track", "year"],
    tagsFromJsmediatags.tags
  );

  return {
    album: tags.album && tags.album.length ? tags.album : null,
    artist: tags.artist && tags.artist.length ? tags.artist : null,
    genre: tags.genre && tags.genre.length ? tags.genre : null,
    nr: tags.track ? tags.track : null,
    title: tags.title && tags.title.length ? tags.title : null,
    year: tags.year && tags.year.length ? getYear(tags.year) : null,
  };
}


//
// Utils

function getYear(dateStr) {
  return (new Date(dateStr)).getFullYear();
}
