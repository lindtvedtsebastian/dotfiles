#!/usr/bin/env bash
set -e  # Exit immediately if any command fails

# Cross-platform packages (macOS + Linux)
COMMON="emacs zsh"

# Linux-only packages (Hyprland desktop)
LINUX="hypr quickshell"

uninstall_packages() {
    for pkg in $1; do
        if [ -d "$pkg" ]; then
            stow --verbose=2 --ignore .DS_Store -D "$pkg"
        fi
    done
}

case "$(uname -s)" in
    Darwin)
        echo "Uninstalling macOS packages..."
        uninstall_packages "$COMMON"
        ;;
    Linux)
        echo "Uninstalling Linux packages..."
        uninstall_packages "$COMMON"
        uninstall_packages "$LINUX"
        ;;
    *)
        echo "Unknown OS, uninstalling common packages only..."
        uninstall_packages "$COMMON"
        ;;
esac

echo "Done."
