# Isotach

A music player which connects to your cloud/distributed storage.

![](https://icidasset-public.s3.amazonaws.com/isotach.jpg)



## Dependencies

- Loads of `Elm`
- A moderate amount of `Haskell` for the build system and static server
- A tiny bit of `Node` (autoprefixer, browserify vendor js, etc.)
- Blockstack


### Setup

```shell
# This assumes that you are using macOS

brew install haskell-stack
brew install elm
brew install nodejs
brew install watchexec

npm install
stack setup
```



## How to run

```shell
# build + server + watch
make

# make a static `./build`
make build

# run the static server
make server

# run tests
make test

# watch and build
make watch
```
