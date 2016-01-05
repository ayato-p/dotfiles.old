=============
 My dotfiles
=============

Setup
=====

wget -O dotfiles.zip https://bit.ly/ayatopdotfileszip && unzip dotfiles.zip && mv dotfiles-master dotfiles && ./dotfiles/setup.sh

after that you'll need to do following...

1. create ssh key
$ ssh-keygen -t rsa -b 4096 -C "my_email@example.com"
$ xclip -sel clip < ~/.ssh/id_rsa.pub

2. install emacs plugins
$ cd .emacs.d && cask install

3. install tmux plugins
Type shortcut key `C-t I`

4. setup fcitx for skk
$ fcitx-configtool

TODO
====

1. add skk jisyo?
2. create makefile
3. some tools can't install now via setup.sh (e.g. peco,tldr,leiningen)
