#!/usr/bin/env bash
# Bootstrap a fresh Mac from this repo. Idempotent.
#
# Usage:
#   ./bootstrap.sh         # full run
#   ./bootstrap.sh brew    # just Homebrew + Brewfile
#   ./bootstrap.sh stow    # just symlink dotfiles

set -euo pipefail

DOTFILES="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PACKAGES=(emacs ghostty zsh)

say() { printf '==> %s\n' "$*"; }

[[ "$(uname -s)" == "Darwin" ]] || { echo "macOS only."; exit 1; }

install_clt() {
  xcode-select -p >/dev/null 2>&1 && return
  say "Installing Xcode Command Line Tools (a GUI prompt will appear)."
  xcode-select --install || true
  echo "Re-run this script once the installer finishes."
  exit 0
}

install_brew() {
  if ! command -v brew >/dev/null; then
    say "Installing Homebrew."
    NONINTERACTIVE=1 /bin/bash -c \
      "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  fi
  [[ -x /opt/homebrew/bin/brew ]] && eval "$(/opt/homebrew/bin/brew shellenv)"
}

run_bundle() {
  say "Running brew bundle."
  brew bundle --file="$DOTFILES/Brewfile"
}

run_stow() {
  command -v stow >/dev/null \
    || { echo "stow missing. Run './bootstrap.sh brew' first."; exit 1; }
  if [[ -d "$HOME/.emacs.d" && ! -L "$HOME/.emacs.d" ]]; then
    backup="$HOME/.emacs.d.bak.$(date +%s)"
    say "Moving existing ~/.emacs.d to $backup"
    mv "$HOME/.emacs.d" "$backup"
  fi
  say "Stowing: ${PACKAGES[*]}"
  cd "$DOTFILES" && stow --target="$HOME" --restow "${PACKAGES[@]}"
}

case "${1:-all}" in
  all)
    install_clt
    install_brew
    run_bundle
    run_stow
    say "Done. Open a new shell. AeroSpace needs Accessibility permission on first launch."
    ;;
  brew) install_brew; run_bundle ;;
  stow) run_stow ;;
  *)    echo "Usage: $0 [all|brew|stow]"; exit 1 ;;
esac
