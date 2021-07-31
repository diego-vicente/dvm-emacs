# `dvm-emacs` - Diego Vicente's personal Emacs configuration

This repository contains my Emacs configuration, configured from scratch
to my personal needs.

As a short introduction and motivation, I have been an Emacs user since
summer 2016. I previously was a Vim user, and my "gateway drug" to Emacs
was [Spacemacs][1]. Few months after starting using it, I started feeling
uncomfortable of how bloated it was and how some things were absolutely
out of my control, lost in the default layers of indirection. The
natural step was to start crafting my own Emacs configuration from
scratch. That effort started in 2017 and now amounts for hundreds of
commits in this repository.

In early 2020 I started experimenting with [doom-emacs][2], until I
finally decided to migrate my configuration to it. It provides some
sensible defaults but does not prevent the user from configuring their
editor just like a vanilla Emacs. I also found that `doom-emacs` did
most of the things I was doing in my config, just faster. After a while,
however, I started feeling like I lost control over my tools: the added
complexity of the framework starting to crept in my configuration and
making me uneasy when performing some deep configuration or fine
tuning.

For that reason, in summer 2021, history repeated again and went through
my third Emacs bankruptcy. Trying to gain back control on my
configuration and aiming for comprehensive configuration and minimalis,
I started writing this current config.

## Installing

The configuration depends on some routes that are defined as variables,
and are expected to be find there. It is important to check the variable
`dvm/user-configuration-directory`, which is by default set to
`~/etc/dvm-emacs`. Set it to the value where you clone this
repository. For activating this configuration, simply make a link to
`init.el`.

```shell
ln -s /path/to/configuration/init.el $HOME/.emacs.d/init.el
```

All packages and dependencies are bootstrapped as `use-package`
definitions and will be automatically installed if they are not found
when starting the editor. It is a plan in the near future to define this
configuration as a [Nix flake][3].

[1]: https://www.spacemacs.org/
[2]: https://github.com/hlissner/doom-emacs
[3]: https://nixos.wiki/wiki/Flakes
