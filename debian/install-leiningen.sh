#!/bin/bash

wget -O ~/bin/lein https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
wget -O ~/.zsh/completion/_lein https://raw.githubusercontent.com/technomancy/leiningen/master/zsh_completion.zsh
chmod a+x ~/bin/lein && lein
