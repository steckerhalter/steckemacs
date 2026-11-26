# steckemacs

[![Build Status](https://github.com/steckerhalter/steckemacs/actions/workflows/ci.yml/badge.svg)](https://github.com/steckerhalter/steckemacs/actions/workflows/ci.yml)

Emacs configuration that fetches and compiles packages directly from source repos via `quelpa2`.

![screenshot](https://raw.github.com/steckerhalter/steckemacs/master/screenshot.png "screen shot")

it uses:

- [outshine](https://github.com/tj64/outshine) for `org-mode`-like document folding: use `C-M-i' to cycle!
- [quelpa2](https://github.com/quelpa/quelpa2), the new package layer for decentralized packaging!

## Setup

**Emacs 30.1 ist required at least**

    cd ~
    git clone https://github.com/steckerhalter/steckemacs.git
    ln -s steckemacs/steckemacs.el .emacs

## Usage

After having symlinked the init file you can fire up Emacs.

It takes quite a while to install all the packages the first time it is run, so you might drink a beer in the meantime.

I usually run Emacs as a daemon:

    emacs --daemon

and then use a shortcut with a command like this to open new frames:

    emacsclient -c --alternate-editor=

This also makes sure to start the Emacs daemon if it's not running already, because

```
-a EDITOR, --alternate-editor=EDITOR
                        Editor to fallback to if the server is not running
                        If EDITOR is the empty string, start Emacs in daemon
                        mode and try connecting again
```

## Local testing

A `Makefile` is included to try the init file without touching your current
setup:

    make          # list the targets
    make run      # start Emacs with steckemacs.el (uses your installed packages)
    make sandbox  # start with a throwaway init dir (.sandbox); installs everything on first run
    make batch    # load the init in batch mode, log to build/startup.log, print elapsed time
    make check    # like batch, but exits non-zero on failure
    make clean    # remove .sandbox and build

Add `EMACS_ARGS=--debug-init` to `run` or `sandbox` to get a backtrace:

    make run EMACS_ARGS=--debug-init

Enjoy!
