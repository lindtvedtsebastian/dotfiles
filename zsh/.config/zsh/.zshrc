source "$ZDOTDIR/options.zsh"

# Machine-specific config (paths, tools, completions)
[ -f "$ZDOTDIR/.zshrc.local" ] && source "$ZDOTDIR/.zshrc.local"

source "$ZDOTDIR/aliases.zsh"
source "$ZDOTDIR/keybindings.zsh"
source "$ZDOTDIR/completion.zsh"
source "$ZDOTDIR/prompt.zsh"
