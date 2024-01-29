export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_PICTURES_DIR="$HOME/.screenshots"

export ZDOTDIR="$XDG_CONFIG_HOME/zsh" # zsh folder
export HISTFILE="$ZDOTDIR/.zhistory" # zsh history
export HISTSIZE=10000 # maximum events for internal history
export SAVEHIST=10000 # maximum events in history file

# increase lsp performance in emacs
export LSP_USE_PLISTS=true

# encoding
export LANG=en_US.UTF-8

# add cargo to path
export PATH=$PATH:$HOME/.cargo/bin
