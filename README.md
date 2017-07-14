# Ongaku Ryoho

A music player which connects to your cloud/distributed storage.

![](http://icidasset-public.s3.amazonaws.com/Screen%20Shot%202017-06-12%20at%207.33.13%20PM.png)



## Dependencies

- Loads of `Elm`
- A moderate amount of `Haskell` for the build system and static server
- A tiny bit of `Node` (for making a separate css file)
- Blockstack


### Setup

```
npm install
```



## How to run

```shell
# make a static `./build`
make

# run the static server
make server

# run tests
make test

# watch and build
brew install watchexec
make watch
```
