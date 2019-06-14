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
- [Textile](https://github.com/textileio/go-textile)

#### Music layer

- [Amazon S3](https://aws.amazon.com/s3/)
- [Azure Blob Storage](https://azure.microsoft.com/en-us/services/storage/blobs/)
- [Azure File Storage](https://azure.microsoft.com/en-us/services/storage/files/)
- [~~Blockstack Gaia Storage~~](https://github.com/blockstack/gaia) <small>(TODO)</small>
- [Dropbox](https://dropbox.com/)
- [Google Drive](https://drive.google.com/)
- [IPFS](https://ipfs.io/)
- [WebDAV](https://en.wikipedia.org/wiki/WebDAV)



---



### Hosting on your own server

Diffuse is a static web application, which means it's just HTML, CSS and Javascript. No REST API, database, or anything backend-related involved. That said, the app does require a HTTP web server so it can have "clean urls" and use service workers (preferably HTTPS). It also requires one special rule, and that is, no matter which HTML page is requested, it should always render the root `200.html` or `index.html` file. `https://diffuse.sh` uses Netlify, which in turn uses the `_redirects` file for this. You can download a pre-build web-only version of Diffuse on the [releases](https://github.com/icidasset/diffuse/releases) page.

In short:
- Diffuse is a static, serverless, web application
- Diffuse requires a HTTP server (prefer HTTPS for service worker)
- Always render the root `200.html` or `index.html` file
- Download a web build on the [releases](https://github.com/icidasset/diffuse/releases) page

```shell
# Example of a nginx configuration
# Disclaimer: I'm not confident this'll actually work,
#             but it should be something along these lines.
location ~ .html$ {
  try_files $uri /200.html;
}
```



---



### Building it yourself

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
