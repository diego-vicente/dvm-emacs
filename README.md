# `dvm-emacs` - Diego Vicente's personal Emacs configuration

[![builds.sr.ht status](https://builds.sr.ht/~diego/dvm-emacs.svg)](https://builds.sr.ht/~diego/dvm-emacs?)

This repository contains all the steps to build and use my personal Emacs
configuration. 

As a short introduction and motivation, I have been an Emacs user since
summer 2016. I previously was a Vim user, and my "gateway drug" to Emacs was
[Spacemacs][3]. Few months after starting using it, I started feeling
uncomfortable of how bloated it was and how some things were absolutely out of
my control, lost in the default layers of indirection. The natural step was to
start crafting my own Emacs configuration from scratch. That effort started in
2017 and now amounts for hundreds of commits in this repository.

In early 2020 I started experimenting with [`doom-emacs`][2], until I finally
decided to migrate my configuration to it. It provides some sensible defaults
but does not prevent the user from configuring their editor just like a vanilla
Emacs. I also found that `doom-emacs` did most of the things I was doing in my
config, just faster. For that reason, this configuration is currently
bootstrapped using `doom-emacs` and all the tools and macros it provides.

## Compiling Emacs

After some frustration trying to download an Emacs version from different `apt`
channels that fit all my requirements, I decided that compiling with flags the
version I needed. For this, I decided to go with the ubiqutuous 26.3 version.
The source code is available in the [GNU FTP][1].

Once the code is available locally, it can be build with these steps:

``` shell
tar -zxvf emacs-26.3.tar.gz
cd emacs-26.3
# Install the dependencies for the build
sudo apt-get build-dep emacs-lucid
# Configure with all the flags needed
./configure --with-x-toolkit=lucid --with-modules
make && sudo make install
```

## Installing and configuring `doom`

To download and install [`doom-emacs`][2] is simple as cloning the repository
into the `.emacs.d` directory. `doom` is able to bootstrap itself and download
all the necessary packages. To 

``` shell
sudo apt-get install git ripgrep tar fd-find clang
# Clone emacs-doom in the emacs directory
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
# Clone this repository in the doom directory
git clone git@git.sr.ht:~diego/dvm-emacs ~/.doom.d
doom install
```


[1]: https://ftp.gnu.org/pub/gnu/emacs/emacs-26.3.tar.gz 
[2]: https://github.com/hlissner/doom-emacs
[3]: https://www.spacemacs.org/
