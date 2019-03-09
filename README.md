<img src="https://diffuse.sh/images/diffuse-dark.svg" alt="Diffuse" width="158" />

_A music player which connects to your cloud/distributed storage,  
in the form of a static, serverless, web application._

📍 Available at [diffuse.sh](https://diffuse.sh/) and for [download](https://github.com/icidasset/diffuse/releases).

<br />
<img src="https://icidasset-public.s3.amazonaws.com/diffuse.jpg" />



### Integrations

[More info](https://diffuse.sh/about/)

#### User layer

- [IndexedDB](https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API) <small>(unencrypted data in browser)</small>
- [IPFS](https://ipfs.io/) <small>(encrypted)</small>

#### Music layer

- [Amazon S3](https://aws.amazon.com/s3/)



---



### Development

For version numbers,  
see `.tool-versions` and `stack.yaml`.

- [Elm](https://elm-lang.org/) programming language
- [Haskell](https://docs.haskellstack.org/en/stable/README/) programming language
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
