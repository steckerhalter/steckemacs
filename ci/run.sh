#!/bin/bash -e

export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

trap "rm -rf ~/.emacs.d/" EXIT

pwd=`pwd`

if ! test -e /usr/local/bin/emacs; then
    sudo apt-get update
    sudo apt-get install build-essential wget
    sudo apt-get build-dep emacs23
    wget http://ftp.gnu.org/gnu/emacs/emacs-24.5.tar.gz
    tar -xzf emacs-24.5.tar.gz
    >&2 echo "--- building emacs ---"
    ./configure &&\
        make &&\
        sudo make install
    sudo apt-get install -qq git mercurial subversion bzr cvs
fi

echo "--- running tests ---"
mkdir ~/.emacs.d
/usr/local/bin/emacs --batch --load $pwd/steckemacs.el
