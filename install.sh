#!/usr/bin/env bash

cd "$( dirname "${BASH_SOURCE[0]}" )"
WD="$(pwd -P)"

# If I ever have a home folder named "*" I deserve what I get.
RWD="${WD#$(cd $HOME; pwd -P)/}"

if [[ "$WD" != "$HOME/.config/dotfiles" ]]; then
    echo
    echo "WARNING! expected to exist in $HOME/.config/dotfiles, actually in $WD"
    echo
fi

PROBLEMS=0

function finish {
    if [[ 0 -eq $? ]]; then
        echo "OK"
    else
        echo "FAILED"
        PROBLEMS=1
    fi
}

function do_git {
    local INSTL="$HOME/$2"
    echo -n "Installing '$1' to '$INSTL'... "
    if [[ -d "$INSTL/.git" ]]; then
        echo "EXISTS, SKIPPING"
    else
        git clone "$1" "$INSTL"
        finish
    fi
}

function do_install {
    local SRC="$RWD/$1"
    local INSTL="$HOME/$2"
    echo -n "Linking '$SRC' to '$INSTL'... "
    if [[ -L "$INSTL" ]]; then
        rm "$INSTL"
    else
        mv "$INSTL" "$INSTL~"
    fi
    ln -s "$SRC" "$INSTL"
    finish
}

do_install emacs .emacs.d
do_install screenrc .screenrc
do_install vimrc .vimrc
do_install gitconfig .gitconfig
do_install gitignore_global .gitignore_global
do_git https://github.com/robbyrussell/oh-my-zsh .oh-my-zsh
do_install oh-my-zsh-custom/zshrc .zshrc
do_install slate .slate
do_install slate.js .slate.js

exit $PROBLEMS
