<img src="https://diffuse.sh/images/diffuse-dark.svg" alt="Diffuse" width="158" />

_A music player that connects to your cloud/distributed storage,
in the form of a static, serverless, web application._

📍 Available at [diffuse.sh](https://diffuse.sh/) and for [download](https://github.com/icidasset/diffuse/releases).

<br />
<img src="https://icidasset-public.s3.amazonaws.com/diffuse-v3.jpg" />



### Integrations

Music layer for music storage.
User layer for user-data storage.

#### Music layer

- [Amazon S3](https://aws.amazon.com/s3/)
- [Azure Blob Storage](https://azure.microsoft.com/en-us/services/storage/blobs/)
- [Azure File Storage](https://azure.microsoft.com/en-us/services/storage/files/)
- [Dropbox](https://dropbox.com/)
- [IPFS](https://ipfs.io/)
- [WebDAV](https://en.wikipedia.org/wiki/WebDAV)

#### User layer

- [Dropbox](https://www.dropbox.com/)
- [IPFS](https://ipfs.io/) <small>(using MFS)</small>
- [RemoteStorage](https://remotestorage.io/)



---



### Hosting on your own server

Diffuse is a static web application, which means it's just HTML, CSS, and Javascript. No REST API, database, or anything backend-related involved. The app uses a hash (aka. fragment-based) routing system, so you don't need any special server rules for routing. You can download a pre-build web-only version of Diffuse on the [releases](https://github.com/icidasset/diffuse/releases) page. Diffuse uses service workers, so you may need HTTPS for it to work smoothly in certain browsers.

I should also note that some source services use OAuth, so you'll need to use your own application credentials (eg. Dropbox). That said, if you're working locally, you can use `http://localhost:8000` or `http://127.0.0.1:44999` to use the default ones, that's what the old Electron app was using.

In short:
- Diffuse is a static, serverless web application
- Routing is done using hashes/fragments (eg. `diffuse.sh/#/sources`)
- Download a web build on the [releases](https://github.com/icidasset/diffuse/releases) page
- Uses service workers (use HTTPS if possible)
- May need own OAuth application credentials for some source services



---



### Building it yourself

This project can be built with [Node.js](https://nodejs.org/).

```shell
# 🍱

# 1. Install dependencies
npm install

# 2. Build
npx just build

# 3. Start static-file server
npx just server

# 4. Watch for changes (requires [watchexec](https://github.com/watchexec/watchexec/) to be installed)
npx just watch

# Alternatively, to build, serve & watch:
npx just
```
