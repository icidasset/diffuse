<img src="https://diffuse.sh/images/diffuse-dark.svg" alt="Diffuse" width="158" />

_A music player that connects to your cloud/distributed storage,  
in the form of a static, serverless, web application._

📍 Available at [diffuse.sh](https://diffuse.sh/) and for [download](https://github.com/icidasset/diffuse/releases).

<br />
<img src="https://icidasset-public.s3.amazonaws.com/diffuse-v2.jpg" />



### Integrations

User layer for user-data storage.  
Music layer for music storage.

#### User layer

- [Blockstack](https://blockstack.org/)
- [Dropbox](https://www.dropbox.com/)
- [IndexedDB](https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API) <small>(browser)</small>
- [IPFS](https://ipfs.io/)
- [RemoteStorage](https://remotestorage.io/)
- [Textile](https://github.com/textileio/go-textile)

#### Music layer

- [Amazon S3](https://aws.amazon.com/s3/)
- [Azure Blob Storage](https://azure.microsoft.com/en-us/services/storage/blobs/)
- [Azure File Storage](https://azure.microsoft.com/en-us/services/storage/files/)
- [Dropbox](https://dropbox.com/)
- [Google Drive](https://drive.google.com/)
- [IPFS](https://ipfs.io/)
- [WebDAV](https://en.wikipedia.org/wiki/WebDAV)



---



### Hosting on your own server

Diffuse is a static web application, which means it's just HTML, CSS and Javascript. No REST API, database, or anything backend-related involved. The app uses a hash, aka. fragment, based routing system, so you don't need any special server rules for routing. You can download a pre-build web-only version of Diffuse on the [releases](https://github.com/icidasset/diffuse/releases) page. Diffuse uses service workers, so you may need HTTPS for it to work smoothly in certain browsers. I should also note that some source services use OAuth, so you'll need to use your own application credentials (eg. google drive client id + secret).

In short:
- Diffuse is a static, serverless, web application
- Routing is done using hashes/fragments (eg. `diffuse.sh/#/sources`)
- Download a web build on the [releases](https://github.com/icidasset/diffuse/releases) page
- Uses service workers (use HTTPS if possible)
- May need own OAuth application credentials for some source services



---



### Building it yourself

For version numbers, see `.tool-versions` and `stack.yaml`.  
All of these, except the last one, can be install using [homebrew](https://brew.sh/).

- [Elm](https://elm-lang.org/) programming language
- [Haskell](https://docs.haskellstack.org/en/stable/README/) programming language
- [Google Closure Compiler](https://github.com/google/closure-compiler#getting-started) minifying assets
- [Devd](https://github.com/cortesi/devd) web server for development (optional)
- [Watchexec](https://github.com/watchexec/watchexec) watching for file changes (optional)
- [Elm Proofread](https://github.com/icidasset/elm-proofread) documentation tests (optional)


```shell
# 🍱

# 1. Install programming languages:
#    Elm 0.19.1 & Haskell (Stack), see links above

# 2. Install vendor dependencies
make install

# 3. Build, start server & watch for changes
make
```

## Contributors

### Code Contributors

This project exists thanks to all the people who contribute. [[Contribute](CONTRIBUTING.md)].
<a href="https://github.com/icidasset/diffuse/graphs/contributors"><img src="https://opencollective.com/diffuse/contributors.svg?width=890&button=false" /></a>

### Financial Contributors

Become a financial contributor and help us sustain our community. [[Contribute](https://opencollective.com/diffuse/contribute)]

#### Individuals

<a href="https://opencollective.com/diffuse"><img src="https://opencollective.com/diffuse/individuals.svg?width=890"></a>

#### Organizations

Support this project with your organization. Your logo will show up here with a link to your website. [[Contribute](https://opencollective.com/diffuse/contribute)]

<a href="https://opencollective.com/diffuse/organization/0/website"><img src="https://opencollective.com/diffuse/organization/0/avatar.svg"></a>
<a href="https://opencollective.com/diffuse/organization/1/website"><img src="https://opencollective.com/diffuse/organization/1/avatar.svg"></a>
<a href="https://opencollective.com/diffuse/organization/2/website"><img src="https://opencollective.com/diffuse/organization/2/avatar.svg"></a>
<a href="https://opencollective.com/diffuse/organization/3/website"><img src="https://opencollective.com/diffuse/organization/3/avatar.svg"></a>
<a href="https://opencollective.com/diffuse/organization/4/website"><img src="https://opencollective.com/diffuse/organization/4/avatar.svg"></a>
<a href="https://opencollective.com/diffuse/organization/5/website"><img src="https://opencollective.com/diffuse/organization/5/avatar.svg"></a>
<a href="https://opencollective.com/diffuse/organization/6/website"><img src="https://opencollective.com/diffuse/organization/6/avatar.svg"></a>
<a href="https://opencollective.com/diffuse/organization/7/website"><img src="https://opencollective.com/diffuse/organization/7/avatar.svg"></a>
<a href="https://opencollective.com/diffuse/organization/8/website"><img src="https://opencollective.com/diffuse/organization/8/avatar.svg"></a>
<a href="https://opencollective.com/diffuse/organization/9/website"><img src="https://opencollective.com/diffuse/organization/9/avatar.svg"></a>
