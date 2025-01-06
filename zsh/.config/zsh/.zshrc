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
fi
