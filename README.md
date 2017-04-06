# Ongaku Ryoho

A music player which connects to your cloud storage.



## Dependencies

- Loads of `Elm`
- A moderate amount of `Haskell` for the build system and static server
- A tiny bit of `Node` (for making a separate css file)


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



## Setting up Firebase

1. Create app
2. Add Google authentication
3. Add the following `Storage` rules

```
service firebase.storage {
  match /users/{userId}/{allPaths=**} {
  	allow read, write: if request.auth != null;
  }
}
```
