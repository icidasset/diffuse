# Diffuse

A music player which connects to your cloud/distributed storage.

![](https://icidasset-public.s3.amazonaws.com/diffuse.jpg)

📍 Available at [https://diffuse.sh/](https://diffuse.sh/)



## Dependencies

- Loads of `Elm`
- A moderate amount of `Haskell` for the build system and static server
- A tiny bit of `Node` (browserify vendor js & doc tests)



## Development

```shell
# This assumes that you are using macOS

brew install haskell-stack
brew install elm
brew install nodejs
brew install watchexec

npm install
stack setup

# Other dependencies
# 1. For building the Electron version
brew install makeicns
brew install imagemagick
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
