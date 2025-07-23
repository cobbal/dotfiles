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
        git clone "$1" --depth 1 "$INSTL"
        finish
    fi
}

function do_install {
    local SRC="$RWD/$1"
    if [[ "$ABSOLUTE" = "1" ]]; then
        SRC="$WD/$1"
    fi
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

ABSOLUTE=1 do_install doom .config/doom
do_install screenrc .screenrc
do_install vimrc .vimrc
do_install gitconfig .gitconfig
do_install gitignore_global .gitignore_global
do_git https://github.com/ohmyzsh/ohmyzsh .config/ohmyzsh
do_git https://github.com/romkatv/powerlevel10k .config/powerlevel10k
do_install oh-my-zsh-custom/zshrc .zshrc
do_install oh-my-zsh-custom/p10k.zsh .p10k.zsh
do_install env/loginitems.sh .loginitems.sh
if [[ $(uname) == Darwin ]]; then
    do_install env/launchd.conf.sh .launchd.conf.sh
    ABSOLUTE=1 do_install env/com.cobbal.environment.plist Library/LaunchAgents/com.cobbal.environment.plist
fi
do_git https://github.com/doomemacs/doomemacs .config/emacs
~/.config/emacs/bin/doom install --no-env --no-config; finish

exit $PROBLEMS
