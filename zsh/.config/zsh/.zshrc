# Machine-specific config (paths, tools, completions)
[ -f "$ZDOTDIR/.zshrc.local" ] && source "$ZDOTDIR/.zshrc.local"

source "$ZDOTDIR/completion.zsh"
source "$ZDOTDIR/prompt.zsh"
