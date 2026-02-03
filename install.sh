#!/usr/bin/env bash
set -e  # Exit immediately if any command fails

# Cross-platform packages (macOS + Linux)
COMMON="emacs zsh"

# Linux-only packages (Hyprland desktop)
LINUX="hypr quickshell"

install_packages() {
    for pkg in $1; do
        stow --verbose=2 --ignore .DS_Store "$pkg"
    done
}

case "$(uname -s)" in
    Darwin)
        echo "Installing macOS packages..."
        install_packages "$COMMON"
        ;;
    Linux)
        echo "Installing Linux packages..."
        install_packages "$COMMON"
        install_packages "$LINUX"
        ;;
    *)
        echo "Unknown OS, installing common packages only..."
        install_packages "$COMMON"
        ;;
esac

echo "Done."
