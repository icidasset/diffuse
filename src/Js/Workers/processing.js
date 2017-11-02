//
// Processing
// (◡ ‿ ◡ ✿)
//
// This code is responsible for extracting metadata out of files.

importScripts("/vendor/package.js");


const tagSet = ["album", "artist", "disc", "genre", "nr", "picture", "title", "year"];


//
// Incoming messages

self.onmessage = event => {
  switch (event.data.action) {
    case "PROCESS_CONTEXT":
      processContext(event.data.context);
      break;
  }
};


//
// Contexts
//

function processContext(context) {
  const initialPromise = Promise.resolve([]);

  return context.urlsForTags.reduce((accumulator, urls) => {
    return accumulator.then(col =>
      getTags(urls.getUrl, urls.headUrl)
        .then(r => col.concat(r))
        .catch(e => {
          console.error(e);
          return col.concat(null);
        })
    );

  }, initialPromise).then(col => {
    context.receivedTags = col.map(
      x => x ? pickTags(x) : null
    );

    self.postMessage({
      action: "PROCESS_CONTEXT",
      context: context
    });

  });
}


//
// Tag reader
// > Override jsmediatags
//

jsmediatags.Config.setXhrTimeoutInSec(5 * 60);
// max request duration: 5 minutes


function getTags(urlGET, urlHEAD) {
  const fakeURL           = "THIS_WONT_BE_USED_ANYWAYS";

  const reader            = new jsmediatags.Reader(fakeURL);
  const fileReader        = new XhrFileReader(fakeURL);
  const makeXHRRequest    = fileReader._makeXHRRequest;
  const setRequestHeader  = fileReader._setRequestHeader;

  // Override fileReader stuff

  fileReader._createXHRObject = function() {
    return new XMLHttpRequest();
  };

  fileReader._setRequestHeader = function(xhr, headerName, headerValue) {
    if (headerName != "If-Modified-Since") {
      setRequestHeader(xhr, headerName, headerValue);
    }
  };

  fileReader._makeXHRRequest = function(method, range, callbacks) {
    this._url = method.toUpperCase() === "HEAD" ? urlHEAD : urlGET;
    return makeXHRRequest.call(this, method, range, callbacks);
  };

  // Get tags

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
    ["album", "artist", "disk", "genre", "picture", "title", "track", "year"],
    tagsFromJsmediatags.tags
  );

  return {
    disc: (tags.disk ? parseInt(tags.disk.data.disk, 10) : 1) || 1,
    nr: (tags.track ? parseInt(tags.track, 10) : 1) || 1,
    album: tags.album && tags.album.length ? tags.album : "Unknown",
    artist: tags.artist && tags.artist.length ? tags.artist : "Unknown",
    title: tags.title && tags.title.length ? tags.title : "Unknown",
    genre: tags.genre && tags.genre.length ? tags.genre : null,
    year: tags.year && tags.year.length ? getYear(tags.year) : null,
    picture: null
  };
}


//
// Pictures (NOTE: disabled for now, slows down the app a lot)

function pictureDataUri(rawPicture) {
  const binary = rawPicture.data.reduce((str, code) => str + String.fromCharCode(code), "");
  return "data:" + rawPicture.format + ";base64," + window.btoa(binary);
}


//
// Utils

function getYear(dateStr) {
  return (new Date(dateStr)).getFullYear();
}
