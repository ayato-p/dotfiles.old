#!/bin/bash

USER=$1
USER_HOME=/home/$USER

SYMLINK_LIST="
.tmux.conf
.xcolors
.Xresources
.Xmodmap
.xsessionrc
.xbindkeysrc
.zshrc
.zshenv
.psqlrc
.gitconfig
.global_gitignore
.spacemacs.d
bin
"

BASE_DIR=$(cd $(dirname $0); pwd)
cd $BASE_DIR

for f in $SYMLINK_LIST
do
    rm -rf "$USER_HOME/$f"
    ln -snfv "$BASE_DIR/$f" "$USER_HOME/$f"
done

cd $HOME

PACKAGE_LIST="
ssh tmux zsh rxvt-unicode-256color
git silversearcher-ag
tree nkf ntpdate imagemagick xclip
build-essential openssl keychain gnupg2 gnupg-agent
autoconf automake libcurl4-gnutls-dev
fonts-inconsolata fonts-takao-gothic fonts-migmix
libskk-dev skkdic skkdic-extra skksearch skktools
libssl-dev libreadline-dev
libxml2-dev libxslt1-dev
curl rlwrap libevent-dev
"
PACKAGE_LIST=`echo $PACKAGE_LIST | sed -e "s/[\r\n]+//g"`

apt install $PACKAGE_LIST

CLJ_VERSION=1.10.0.411
curl -o /opt/clojure-install.sh -O https://download.clojure.org/install/linux-install-$CLJ_VERSION.sh && chmod +x /opt/clojure-install.sh && /opt/clojure-install.sh
