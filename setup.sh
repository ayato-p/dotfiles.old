#!/bin/bash
#------------------------------------------------------
# apt update
#------------------------------------------------------

sudo apt-get update -y
sudo apt-get upgrade -y

#------------------------------------------------------
# deploy dotfiles
#------------------------------------------------------

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
.gitconfig
.global_gitignore
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

#------------------------------------------------------
# create user dirs
#------------------------------------------------------
mkdir ~/.zsh.d
mkdir ~/screenshots
mkdir ~/projects
mkdir ~/lib

#------------------------------------------------------
# install some useful tools
#------------------------------------------------------
sudo apt-get install -y xdotool xbindkeys
sudo apt-get install -y openjdk-8-jdk sbcl
sudo apt-get install -y ssh tmux zsh rxvt-unicode-256color stumpwm
sudo apt-get install -y git silversearcher-ag
sudo apt-get install -y curl tree nkf ntpdate imagemagick xclip
sudo apt-get install -y build-essential openssl keychain gnupg2 gnupg-agent
sudo apt-get install -y autoconf automake libcurl4-gnutls-dev
sudo apt-get install -y emacs25-lucid
sudo apt-get install -y fonts-inconsolata fonts-takao-gothic fonts-migmix
sudo apt-get install -y pandoc
sudo apt-get install -y libskk-dev skkdic skkdic-extra
sudo apt-get install -y ibus ibus-skk skksearch skktools
sudo apt-get install -y libssl-dev libreadline6-dev # for ruby build
sudo apt-get install -y libxml2-dev libxslt1-dev    # for tinkerer
# sudo apt-get install -y mecab libmecab-dev mecab-ipadic-utf8 xz-utils # for mecab
sudo apt-get install -y inotify-tools

# tmux plugin manager
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# xcape
git clone https://github.com/alols/xcape.git
cd xcape && make && sudo make install && cd ~/ && rm -rf xcape

# zplug: zshell plugin manager
curl -sL --proto-redir -all,https https://zplug.sh/installer | zsh

# language tools
# zsh
# theme
git clone https://github.com/sindresorhus/pure.git ~/.zsh.d/pure
cd ~/.zsh.d/pure
sudo ln -s "$PWD/pure.zsh" /usr/local/share/zsh/site-functions/prompt_pure_setup
sudo ln -s "$PWD/async.zsh" /usr/local/share/zsh/site-functions/async
cd ~/

# fzf
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install --key-bindings --completion --no-update-rc --64

# mecab
# git clone --depth 1 https://github.com/neologd/mecab-ipadic-neologd.git
# cd mecab-ipadic-neologd && ./bin/install-mecab-ipadic-neologd -n && cd ~/

# rbenv
git clone https://github.com/rbenv/rbenv.git ~/.rbenv
git clone https://github.com/rbenv/ruby-build.git ~/.rbenv/plugins/ruby-build
cd ~/.rbenv && src/configure && make -C src && cd ~/

# nodenv
git clone https://github.com/nodenv/nodenv.git ~/.nodenv
cd ~/.nodenv && src/configure && make -C src && cd ~/
git clone https://github.com/nodenv/node-build.git ~/.nodenv/plugins/node-build

# pyenv
git clone https://github.com/yyuu/pyenv.git ~/.pyenv
git clone https://github.com/yyuu/pyenv-pip-rehash.git ~/.pyenv/plugins/pyenv-pip-rehash

# go
# wget -O golang.tar.gz https://storage.googleapis.com/golang/go1.5.2.linux-amd64.tar.gz
# sudo tar -C /usr/local -xzf golang.tar.gz && rm golang.tar.gz
# # some useful tools written in go lang
# GOPATH=$HOME/go go get github.com/peco/peco/cmd/peco
# GOPATH=$HOME/go go get github.com/pranavraja/tldr

# roswell for common lisp
git clone -b release https://github.com/roswell/roswell.git
cd roswell && sh bootstrap && ./configure && make && sudo make install && cd .. && rm -rf roswell
ros install sbcl-bin

# leiningen for clojure
wget -O ~/bin/lein https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
sudo wget -O /usr/local/share/zsh/site-functions/_lein https://raw.githubusercontent.com/technomancy/leiningen/master/zsh_completion.zsh
chmod a+x ~/bin/lein && lein

# sassc
cd lib
git clone https://github.com/sass/sassc.git --branch 3.2.2 --depth 1
git clone https://github.com/sass/libsass.git --branch 3.2.2 --depth 1
cd sassc && SASS_LIBSASS_PATH="~/lib/libsass"; make && cd $HOME && ln -s $HOME/lib/sassc/bin/sassc $HOME/bin

# urxvt
# wget -O .urxvt/ext/resize-font https://raw.githubusercontent.com/simmel/urxvt-resize-font/master/resize-font
curl --create-dirs -o .urxvt/ext/resize-font https://raw.githubusercontent.com/simmel/urxvt-resize-font/master/resize-font
git clone https://aur.archlinux.org/urxvt-fullscreen.git ~/.urxvt/ext/fullscreen
# install for screencast
# sudo apt-get install gtk-recordmydesktop -y
# sudo add-apt-repository ppa:pcf/miro-releases
# sudo apt-get update -y && sudo apt-get upgrade -y
# sudo apt-get install miro -y

#------------------------------------------------------
# for Emacs
#------------------------------------------------------
# cask
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
# cmigemo
# git clone https://github.com/koron/cmigemo.git
# cd cmigemo && ./configure && make gcc && make gcc-dict && sudo make gcc-install && cd .. && rm -rf cmigemo

#------------------------------------------------------
# change login shell to zsh
#------------------------------------------------------
sudo chsh -s /bin/zsh && chsh -s /bin/zsh
