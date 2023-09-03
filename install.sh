#!/bin/bash
stow -v zsh
source ~/dotfiles/scripts/tangle_org_file.sh emacs.org
source ~/dotfiles/scripts/tangle_org_file.sh xmonad.org
source ~/dotfiles/scripts/tangle_org_file.sh gnupg.org
source ~/dotfiles/scripts/tangle_org_file.sh xinit.org 
