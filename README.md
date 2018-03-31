<strong><img src="src/Static/Images/diffuse-dark.svg" alt="Diffuse" width="158" /></strong>

A music player which connects to your cloud/distributed storage.

<br />
<img src="https://icidasset-public.s3.amazonaws.com/diffuse.jpg" />

📍 Available at [https://diffuse.sh/](https://diffuse.sh/)



## Versions

🌍 [Web](https://diffuse.sh/)  
Chrome, Firefox, Safari & Edge.

🖥 [Native](https://github.com/icidasset/diffuse/releases)  
MacOS, Linux & Windows.  
<small>Unique features: Add music from local filesystem & media-keys support.</small>



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
