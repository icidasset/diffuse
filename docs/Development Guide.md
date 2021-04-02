Contributed by [@Netherquark](https://github.com/Netherquark)

Hello there! I see you want to build Diffuse.
<p> The process can seem quite complicated, especially if you aren't familiar with concepts like Nix, just like I wasn't when I started out. Don't worry, this guide is here to help you. </p>
<p> First off, determine your operating system (of choice). The setup process is considerably easier on MacOS and Linux, but it is still theoretically possible on Windows. </p>

**Prerequisites for MacOS and Linux**  
1. Curl
2. Basic CLI operation knowledge

**Linux guide** [Tested on Ubuntu 20.10]
1. Set up Git. (https://www.freecodecamp.org/news/learn-the-basics-of-git-in-under-10-minutes-da548267cc91/)
2. Clone Diffuse with `git clone https://www.github.com/icidasset/diffuse`
3. Install Nix using `curl -L https://nixos.org/nix/install | sh`
4. Reboot (recommended)
5. Navigate to the Diffuse folder using `cd`
6. Execute `nix-shell`
7. Execute `just install-deps` after the previous step finishes
8. Execute `just`.
9. ???

**MacOS guide**  
lol idk maybe you write this?

**Prerequisites for Windows**  
1. Git for Windows (https://gitforwindows.org/)
2. Haskell Tool Stack (https://docs.haskellstack.org/en/stable/install_and_upgrade/)
3. Basic CLI operation knowledge
4. `just` (https://github.com/casey/just#installation)
5. NodeJS (https://nodejs.org/en/download/)
6. NPM (https://pnpm.js.org/installation)
7. Elm (https://guide.elm-lang.org/install/elm.html)

**Windows guide(Untested)**  
1. Install all the prerequisites and set them up.
2. ???

In case you have any queries or issues, feel free to reach out to me @Netherquark or to Steven @icidasset.
