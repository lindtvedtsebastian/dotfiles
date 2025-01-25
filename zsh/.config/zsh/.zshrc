#!/bin/bash 
source "$ZDOTDIR/colors.zsh"
source "$ZDOTDIR/completion.zsh"
source "$ZDOTDIR/prompt.zsh"
source "$ZDOTDIR/homebrew.zsh"

if [[ "$OSTYPE" != "darwin"* ]]; then
    eval "$(direnv hook zsh)"
fi

if [[ "$OSTYPE" == "darwin"* ]]; then
    eval "$(rbenv init - --no-rehash zsh)"
    export PATH="/opt/homebrew/opt/llvm/bin/:$PATH"
    export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"
    export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"
fi
