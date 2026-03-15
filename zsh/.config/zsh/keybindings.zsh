# Emacs mode (explicit)
bindkey -e

# Word navigation (Option+f, Option+b, Option+d)
bindkey '\ef' forward-word
bindkey '\eb' backward-word
bindkey '\ed' kill-word

# Home / End / Delete
bindkey '\e[H'  beginning-of-line
bindkey '\e[F'  end-of-line
bindkey '\e[3~' delete-char

# Prefix-based history search (type a prefix, then Up/Down to filter)
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey '\e[A' up-line-or-beginning-search
bindkey '\e[B' down-line-or-beginning-search
