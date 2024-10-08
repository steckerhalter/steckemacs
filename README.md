# steckemacs

**steckEmacs** is an init file for Emacs.

![screenshot](https://raw.github.com/steckerhalter/steckemacs/master/screenshot.png "screen shot")

it uses:

- [use-package](https://github.com/jwiegley/use-package)  to structure the code into logical units
- [quelpa](https://github.com/quelpa/quelpa) to install and compile all packages from source code
- [outshine](https://github.com/tj64/outshine) for `org-mode`-like document folding: use `C-M-i' to cycle

## Setup

**Emacs 24.4 ist required at least**

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

Enjoy!
