# steckemacs.el

`steckemacs` Emacs configuration.

What was once `steckemacs` with org-mode tangling is now just an Elisp file.

## Setup

**Emacs 24.4 ist required at least**

    cd ~
    git clone https://github.com/steckerhalter/steckemacs.el.git
    ln -s steckemacs.el/steckemacs.el .emacs

## Usage

[outshine](https://github.com/tj64/outshine) is used to navigate and structure the document (see there for more infos).

I usually run Emacs as a daemon so on startup I run with a script:

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

Beware that on the first start it can take a long time since all the packages are fetched and installed from the internet (**you need the internets!**) with [quelpa](https://github.com/quelpa/quelpa).
