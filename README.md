<img src="https://diffuse.sh/images/diffuse-dark.svg" alt="Diffuse" width="158" />

__A music player which connects to your cloud/distributed storage__, in the form of a static, serverless, web application.

<br />
<img src="https://icidasset-public.s3.amazonaws.com/diffuse.jpg" />

📍 Available at [https://diffuse.sh/](https://diffuse.sh/)



## Integrations

[More info](https://diffuse.sh/about/)

#### User layer

- [IndexedDB](https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API) <small>(ie. anonymous mode)</small>

#### Music layer

- [Amazon S3](https://aws.amazon.com/s3/)



---



## Development

- [Elm](https://elm-lang.org/) programming language
- [Haskell](https://docs.haskellstack.org/en/stable/README/) programming language
- [Elm Proofread](https://github.com/icidasset/elm-proofread) documentation tests
- [Devd](https://github.com/cortesi/devd) web server for development
- [Watchexec](https://github.com/watchexec/watchexec) watching for file changes


```shell
# build, start server & watch for changes
make

# make a static ./build
make build

# run tests
make test
```
