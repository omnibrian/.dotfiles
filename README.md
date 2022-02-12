# .dotfiles

There's no place like 127.0.0.1

## Installation

Using dotbot, the makefile handles setting up dotbot and running it to install dots:

```
make
make install
```

## Non-dotfile config

Since some programs use dconf for configuration, their config files are just a dconf dump. They can be updated with the dconf-dump make target:

```
make dconf-dump
```

**Note:** `make install` will read and dconf load those files so current config may get overwritten if running `make install` before `make dconf-dump`.
