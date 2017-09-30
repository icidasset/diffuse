# Ongaku Ryoho

A music player which connects to your cloud/distributed storage.

![](http://icidasset-public.s3.amazonaws.com/ongakuryoho.jpg)



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

npm install
make elm-install
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
brew install watchexec
make watch
```
