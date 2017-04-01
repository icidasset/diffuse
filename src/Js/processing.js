//
// Tag reader

function getTags(urlGET, urlHEAD) {
  const fakeURL = 'STOP_ME_FROM_DOING_EVIL';

  const reader = new jsmediatags.Reader(fakeURL);
  const fileReader = new XhrFileReader(fakeURL);
  const makeXHRRequest = fileReader._makeXHRRequest;

  fileReader._createXHRObject = function() {
    return new XMLHttpRequest();
  };

  fileReader._makeXHRRequest = function(method, ...args) {
    this._url = method.toUpperCase() === 'HEAD' ? urlHEAD : urlGET;
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



function pickTags(tagsFromJsmediatags) {
  const tags = _.pick(
    ["album", "artist", "genre", "title", "track", "year"],
    tagsFromJsmediatags.tags
  );

  return {
    album: tags.album && tags.album.length ? tags.album : null,
    artist: tags.artist && tags.artist.length ? tags.artist : null,
    genre: tags.genre && tags.genre.length ? tags.genre : null,
    title: tags.title && tags.title.length ? tags.title : null,
    track: tags.track && tags.track.length ? parseInt(tags.track, 10) : null,
    year: tags.year && tags.year.length ? parseInt(tags.year, 10) : null,
  };
}
