=============
 My dotfiles
=============

wget -O dotfiles.zip https://bit.ly/ayatopdotfileszip && unzip dotfiles.zip && mv dotfiles-master dotfiles && ./dotfiles/setup.sh

after that you'll need to do following...

ssh-keygen -t rsa -b 4096 -C "your_email@example.com"
xclip -sel clip < ~/.ssh/id_rsa.pub
