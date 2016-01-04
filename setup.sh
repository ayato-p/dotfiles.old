#!/bin/bash
#------------------------------------------------------
# apt update
#------------------------------------------------------
# use japan repo; http://pc.casey.jp/archives/153900744
sudo sed -i".bak" -e 's/\/\/us.archive.ubuntu.com/\/\/ubuntutym.u-toyama.ac.jp/g' /etc/apt/sources.list

# for google chrome
sudo wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
sudo sh -c 'echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list'

sudo apt-get update -y
sudo apt-get upgrade -y

#------------------------------------------------------
# create user dirs
#------------------------------------------------------
mkdir ~/bin
mkdir ~/.zsh.d
mkdir ~/screenshots

#------------------------------------------------------
# install some useful tools
#------------------------------------------------------
sudo apt-get install -y ssh tmux zsh rxvt-unicode-256color stumpwm
sudo apt-get install -y git silversearcher-ag
sudo apt-get install -y curl tree nkf ntpdate imagemagick xclip
sudo apt-get install -y build-essential openssl
sudo apt-get install -y autoconf automake libcurl4-gnutls-dev
sudo apt-get install -y emacs
sudo apt-get install -y fonts-inconsolata fonts-takao-gothic
sudo apt-get install -y openjdk-8-jdk # sbcl
sudo apt-get install -y google-chrome-stable
sudo apt-get install -y fcitx libskk-dev skkdic skkdic-extra fcitx-skk ddskk

# tmux plugin manager
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# language tools
# zsh
# theme
git clone https://github.com/sindresorhus/pure.git ~/.zsh.d/pure
cd ~/.zsh.d/pure
sudo ln -s "$PWD/pure.zsh" /usr/local/share/zsh/site-functions/prompt_pure_setup
sudo ln -s "$PWD/async.zsh" /usr/local/share/zsh/site-functions/async
cd ~/

# rbenv
git clone https://github.com/rbenv/rbenv.git ~/.rbenv
git clone https://github.com/rbenv/ruby-build.git ~/.rbenv/plugins/ruby-build
cd ~/.rbenv && src/configure && make -C src && cd ..
# ndenv
git clone https://github.com/riywo/ndenv ~/.ndenv
git clone https://github.com/riywo/node-build.git $(ndenv root)/plugins/node-build
# go
wget -O golang.tar.gz https://storage.googleapis.com/golang/go1.5.2.linux-amd64.tar.gz
sudo tar -C /usr/local -xzf golang.tar.gz && rm golang.tar.gz
# some useful tools written in go lang
/usr/local/go/bin/go get github.com/peco/peco/cmd/peco
/usr/local/go/bin/go get github.com/pranavraja/tldr
# common lisp
git clone -b release https://github.com/snmsts/roswell.git
cd roswell && sh bootstrap && ./configure && make && sudo make install && cd .. && rm -rf roswell
ros install sbcl-bin
# leiningen for clojure
wget -O ~/bin/lein https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
sudo wget -O /usr/local/share/zsh/site-functions/_lein https://raw.githubusercontent.com/technomancy/leiningen/master/zsh_completion.zsh
chmod a+x ~/bin/lein && lein

#------------------------------------------------------
# download my config files
#------------------------------------------------------
# wget https://gist.githubusercontent.com/ayato-p/9988b9385f41f348fbe5/raw/116274b4cb8c52d15fa3efafa2a2efc9d7ea5d68/.zshrc

#------------------------------------------------------
# for Emacs
#------------------------------------------------------
# cask
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
# cmigemo
git clone https://github.com/koron/cmigemo.git
cd cmigemo && ./configure && make gcc && make gcc-dict && sudo make gcc-install && cd .. && rm -rf cmigemo
# my emacs conf
git clone https://ayato_p@bitbucket.org/ayato_p/dotemacs-for-clojure.git .emacs.d

#------------------------------------------------------
# deploy dotfiles
#------------------------------------------------------

SYMLINK_LIST="
.emacs.d
.tmux.conf
.xcolors
.Xresources
.xsessionrc
.zshrc
.stumpwm
bin
"
BASE_DIR=$(cd $(dirname $0); pwd)
cd $BASE_DIR

for f in $SYMLINK_LIST
do
    rm -rf ~/$f
    ln -snfv "$BASE_DIR/$f" "$HOME"/"$f"
done

#------------------------------------------------------
# change login shell to zsh
#------------------------------------------------------
sudo chsh -s /bin/zsh && chsh -s /bin/zsh
