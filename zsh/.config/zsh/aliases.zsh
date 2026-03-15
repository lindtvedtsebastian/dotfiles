# ls (auto-detect GNU coreutils vs BSD)
if command -v gls &>/dev/null; then
    alias ls='gls --color=auto --group-directories-first'
else
    alias ls='ls -G'
fi
alias ll='ls -lh'
alias la='ls -lAh'

# Safety nets
alias rm='rm -i'
alias mv='mv -i'
alias cp='cp -i'

# Directories
alias mkdir='mkdir -p'
alias ..='cd ..'
alias ...='cd ../..'
alias d='dirs -v'

# Shell
alias reload='source "$ZDOTDIR/.zshrc"'
