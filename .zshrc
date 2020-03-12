# Key binding - Emacs
bindkey -e
# Disable to default C-q
stty start undef

# for Hisotry
HISTSIZE=9999
SAVEHIST=9999
HISTFILE=~/.zsh_history
setopt histignorealldups hist_reduce_blanks hist_no_store hist_verify share_history

# for Prompt
autoload -Uz vcs_info
autoload -U promptinit
setopt prompt_subst
promptinit

PURE_PROMPT_SYMBOL="λ"

zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "%F{yellow}!%f"
zstyle ':vcs_info:git:*' unstagedstr "%F{red}+%f"
zstyle ':vcs_info:*' formats "%c%u%F{cyan}**%f%B%F{white}>%f%F{black}(%f%F{green}%b%f%F{black})%f"
zstyle ':vcs_info:*' actionformats '[%b|%a]'
precmd () {
    vcs_info
}
# RPROMPT='${vcs_info_msg_0_}'

# for Completion
[[ -d ~/.zsh/completion ]] || mkdir -p ~/.zsh/completion
export fpath=(~/.zsh/completion $fpath)
autoload -Uz compinit
compinit

setopt complete_in_word

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'


# User definition

alias gpg="gpg2"
alias emacsd="emacs &"
alias ekill='emacsclient -e "(kill-emacs)"'
alias e="emacsclient -nw"
alias cdd="find . -maxdepth 1 -type d | fzf | cd"


if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    #alias fgrep='fgrep --color=auto'
    #alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'



# export http_proxy=http://localhost:8000
# export https_proxy=http://localhost:8000

export EDITOR='emacsclient -nw'
export TERM=screen-256color

export PATH=$HOME/bin:$PATH
export PATH=$HOME/.cask/bin:$PATH

source <(minikube completion zsh)
source <(kubectl completion zsh)

# Language tools
export PATH=$PATH:$HOME/.cargo/bin

# for Go lang
export GOPATH=$HOME/opt/golang
export PATH=$PATH:$GOPATH/language/go_1.13.3/bin:$GOPATH/bin
export GO111MODULE=on


# for Leiningen(Clojure)
export LEIN_GPG=gpg2

export PATH=$HOME/lib/jbake/bin:$PATH
export PATH=$HOME/lib/redpen/bin:$PATH
export LD_LIBRARY_PATH=$HOME/lib/hornetq

# for stack(haskell)
export PATH=$HOME/.local/bin:$PATH

export PATH=$HOME/.nodenv/bin:$PATH
[[ -n `command -v nodenv` ]] && eval "$(nodenv init -)"

export PATH="$HOME/.rbenv/bin:$PATH"
[[ -n `command -v rbenv` ]] && eval "$(rbenv init -)"

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
[[ -n `command -v pyenv` ]] && eval "$(pyenv init -)" && eval "$(pyenv virtualenv-init -)"

export SASS_LIBSASS_PATH="$HOME/lib/libsass"

# Stop warning with Emacs
export NO_AT_BRIDGE=1

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

# po4a
export PERLLIB="/home/ayato-p/opt/po4a-0.56/lib"

# For fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_OPTS="--reverse --inline-info --bind=ctrl-k:kill-line"


# zplug
export ZPLUG_HOME=$HOME/.zplug
[[ -d $ZPLUG_HOME ]] || {
  git clone https://github.com/b4b4r07/zplug ~/.zplug
  source ~/.zplug/init.zsh && zplug update --self
}

source $ZPLUG_HOME/init.zsh

zplug "b4b4r07/zplug"
# zplug "junegunn/fzf-bin", as:command, from:gh-r, rename-to:fzf
# zplug "junegunn/fzf", as:command, use:bin/fzf-tmux
zplug "b4b4r07/enhancd", use:init.sh
export ENHANCD_FILTER=fzf-tmux:fzf
zplug "zsh-users/zsh-completions", depth:1
zplug "sindresorhus/pure", use:pure.zsh, from:github, as:theme
export PURE_THEME_DIR=$ZPLUG_HOME/repos/sindresorhus/pure

[[ -f ~/.zsh/completion/prompt_pure_setup ]] || {
    ln -s $PURE_THEME_DIR/pure.zsh ~/.zsh/completion/prompt_pure_setup
}
[[ -f ~/.zsh/completion/async ]] || {
    ln -s $PURE_THEME_DIR/async.zsh ~/.zsh/completion/async
}

if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    else
        echo
    fi
fi

zplug load

# tmux
if [ -z $TMUX ] ; then
    if [ -z `tmux ls 2> /dev/null` ] ; then
        tmux
    else
        tmux attach
    fi
else
    kuromadoushi
fi

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/ayato-p/.sdkman"
[[ -s "/home/ayato-p/.sdkman/bin/sdkman-init.sh" ]] && source "/home/ayato-p/.sdkman/bin/sdkman-init.sh"

# opam configuration
test -r /home/ayato-p/.opam/opam-init/init.zsh && . /home/ayato-p/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/ayato-p/opt/google-cloud-sdk-245/path.zsh.inc' ]; then . '/home/ayato-p/opt/google-cloud-sdk-245/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/ayato-p/opt/google-cloud-sdk-245/completion.zsh.inc' ]; then . '/home/ayato-p/opt/google-cloud-sdk-245/completion.zsh.inc'; fi
