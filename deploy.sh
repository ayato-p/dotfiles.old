#!/bin/bash

SYMLINK_LIST="
.emacs.d
.tmux.conf
.xcolors
.Xresources
.Xmodmap
.xsessionrc
.xbindkeysrc
.zshrc
.zshenv
.stumpwmrc
.psqlrc
bin
"

BASE_DIR=$(cd $(dirname $0); pwd)
cd $BASE_DIR

for f in $SYMLINK_LIST
do
    rm -rf "$HOME/$f"
    ln -snfv "$BASE_DIR/$f" "$HOME/$f"
done

cd $HOME
