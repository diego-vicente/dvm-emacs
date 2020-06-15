# `dvm-emacs` - Diego Vicente's personal Emacs configuration

This repository contains all the steps to build and use my personal Emacs
configuration. 

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
git clone git@git.sr.ht:~diego/dvm-emacs
doom install
```


[1](https://ftp.gnu.org/pub/gnu/emacs/emacs-26.3.tar.gz) 
[2](https://github.com/hlissner/doom-emacs)
