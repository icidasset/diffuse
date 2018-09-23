<strong><img src="https://diffuse.sh/images/diffuse-dark.svg" alt="Diffuse" width="158" /></strong>

A music player which connects to your cloud/distributed storage.

<br />
<img src="https://icidasset-public.s3.amazonaws.com/diffuse.jpg" />

📍 Available at [https://diffuse.sh/](https://diffuse.sh/)



## Versions

🌍 [Web](https://diffuse.sh/)  
Chrome, Firefox, Safari & Edge.

🖥 [Native](https://github.com/icidasset/diffuse/releases)  
MacOS, Linux & Windows.  
<small>Unique features: Add music from local filesystem, WebDAV & media-keys support.</small>



## Integrations

[More info](https://diffuse.sh/about/)

#### User layer

- [Blockstack](https://blockstack.org/)
- [RemoteStorage](remotestorage.io)
- [IndexedDB](https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API) <small>(ie. anonymous mode)</small>

#### Music layer

- [Amazon S3](https://aws.amazon.com/s3/)
- [Azure Blob Storage](https://azure.microsoft.com/en-us/services/storage/blobs/)
- [Dropbox](https://dropbox.com/)
- [Google Drive](https://drive.google.com/)
- [IPFS](https://ipfs.io/)
- Local Filesystem <small>(native only)</small>
- [WebDAV](https://en.wikipedia.org/wiki/WebDAV) <small>(native only)</small>



## Q&A


__Why don't you support WebDAV on the web version?__  
Most WebDAV servers aren't built according to the [CORS](https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS) specification.
Which is a must-have for the web version, otherwise it doesn't work.
The WebDAV version was built with [NextCloud](https://nextcloud.com/) in mind.



## Dependencies

- Loads of `Elm`
- A moderate amount of `Haskell`



## Development

```shell
# This assumes that you are using macOS

brew install haskell-stack
brew install elm

stack setup
```

### How to run

```shell
# build + server + watch
make

# make a static `./build`
make build

# run tests
make test
```
