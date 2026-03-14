#!/bin/bash
source "$ZDOTDIR/colors.zsh"

# Machine-specific config (paths, tools, completions)
[ -f "$ZDOTDIR/.zshrc.local" ] && source "$ZDOTDIR/.zshrc.local"

# EAT (Emulate A Terminal) shell integration
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/zsh"
source "$ZDOTDIR/completion.zsh"
source "$ZDOTDIR/prompt.zsh"
