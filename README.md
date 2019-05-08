<img src="https://diffuse.sh/images/diffuse-dark.svg" alt="Diffuse" width="158" />

_A music player that connects to your cloud/distributed storage,  
in the form of a static, serverless, web application._

📍 Available at [diffuse.sh](https://diffuse.sh/) and for [download](https://github.com/icidasset/diffuse/releases).

<br />
<img src="https://icidasset-public.s3.amazonaws.com/diffuse-v2-alpha.jpg" />



### Integrations

User layer for user-data storage.  
Music layer for music storage.

#### User layer

- [~~Blockstack~~](https://blockstack.org/) <small>(TODO)</small>
- [IndexedDB](https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API) <small>(browser)</small>
- [IPFS](https://ipfs.io/)
- [RemoteStorage](https://remotestorage.io/)
- [~~Solid~~](https://solid.inrupt.com/) <small>(TODO)</small>
- [Textile](https://github.com/textileio/go-textile)

#### Music layer

- [Amazon S3](https://aws.amazon.com/s3/)
- [Azure Blob Storage](https://azure.microsoft.com/en-us/services/storage/blobs/)
- [Azure File Storage](https://azure.microsoft.com/en-us/services/storage/files/)
- [~~Blockstack Gaia Storage~~](https://github.com/blockstack/gaia) <small>(TODO)</small>
- [Dropbox](https://dropbox.com/)
- [Google Drive](https://drive.google.com/)
- ~~HTTP Server~~ <small>(TODO)</small>
- [IPFS](https://ipfs.io/)
- [WebDAV](https://en.wikipedia.org/wiki/WebDAV)



---



### Development

For version numbers,  
see `.tool-versions` and `stack.yaml`.

- [Elm](https://elm-lang.org/) programming language
- [Haskell](https://docs.haskellstack.org/en/stable/README/) programming language
- [Google Closure Compiler](https://github.com/google/closure-compiler#getting-started) minifying assets
- [Elm Proofread](https://github.com/icidasset/elm-proofread) documentation tests (optional)
- [Devd](https://github.com/cortesi/devd) web server for development (optional)
- [Watchexec](https://github.com/watchexec/watchexec) watching for file changes (optional)


```shell
# 🍱

# 1. Install programming languages:
#    Elm 0.19 & Haskell (Stack), see links above

# 2. Install vendor dependencies
make install

# 3. Build, start server & watch for changes
make
```
