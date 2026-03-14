# dotfiles

Personal configuration files managed with [GNU Stow](https://www.gnu.org/software/stow/).

## Structure

```
.
├── emacs/          # Emacs configuration (cross-platform)
├── zsh/            # Zsh shell configuration (cross-platform)
├── hypr/           # Hyprland window manager (Linux)
├── quickshell/     # Quickshell status bar (Linux/Hyprland)
├── nixos/          # NixOS system configuration (Linux)
└── flake.nix       # NixOS flake entry point
```

## Quick Start

### macOS

```bash
# Install stow if needed
brew install stow

# Clone and install
git clone <repo> ~/dotfiles && cd ~/dotfiles
stow emacs zsh
```

### NixOS

```bash
# Clone to home directory
git clone <repo> ~/dotfiles && cd ~/dotfiles

# Stow user configs
stow emacs zsh hypr quickshell

# Apply system configuration
sudo nixos-rebuild switch --flake .#desktop
```

## Machine-specific configuration

Machine-specific shell config (tool paths, completions, environment) goes in `zsh/.config/zsh/.zshrc.local`. This file is gitignored and sourced automatically if present.

## Packages

### emacs (cross-platform)

Modular Emacs configuration using [straight.el](https://github.com/radian-software/straight.el) for package management.

**Targets:** `~/.emacs.d/`

| File | Purpose |
|------|---------|
| `init.el` | Bootstrap and load modules |
| `early-init.el` | Pre-GUI initialization |
| `src/core.el` | Basic settings, UI, behavior |
| `src/dev.el` | LSP, Rust, Go, Python, JS/TS, Java, C/C++ |
| `src/bindings.el` | Keybindings |
| `src/completion.el` | Vertico, Consult, Corfu |
| `src/visual.el` | Theme configuration |
| `src/modeline.el` | Modeline setup |
| `src/utils.el` | Utility functions |
| `src/env.el` | Environment variables |
| `themes/` | Custom themes (nimbus, balanced) |

### zsh (cross-platform)

Minimal zsh configuration with XDG compliance.

**Targets:** `~/.zshenv`, `~/.config/zsh/`

| File | Purpose |
|------|---------|
| `.zshenv` | Environment variables, XDG paths |
| `.zshrc` | Main shell configuration |
| `.zshrc.local` | Machine-specific config (gitignored) |
| `prompt.zsh` | Git-aware prompt with status indicators |
| `completion.zsh` | Completion settings |
| `colors.zsh` | Color definitions |

### hypr (Linux)

[Hyprland](https://hyprland.org/) window manager configuration for a dual-monitor 2560x1440 setup.

**Targets:** `~/.config/hypr/`

- `hyprland.conf` - Main configuration
- `hyprlock.conf` - Lock screen
- `hypridle.conf` - Idle management

### quickshell (Linux)

[Quickshell](https://quickshell.outfoxxed.me/) QML-based status bar for Hyprland.

**Targets:** `~/.config/quickshell/`

### nixos (Linux)

NixOS system configuration with flakes and home-manager.

| Path | Purpose |
|------|---------|
| `hosts/desktop/` | Desktop (AMD GPU) |
| `hosts/laptop/` | Laptop |
| `modules/general.nix` | Common packages |
| `modules/hyprland.nix` | Hyprland setup |
| `modules/amd.nix` | AMD GPU drivers |

**Rebuild:**
```bash
sudo nixos-rebuild switch --flake .#desktop
# or
sudo nixos-rebuild switch --flake .#laptop
```

## Manual Installation

Install individual packages with stow:

```bash
# Single package
stow emacs

# Multiple packages
stow emacs zsh

# Remove symlinks
stow -D emacs
```

## Platform Matrix

| Package | macOS | NixOS |
|---------|:-----:|:-----:|
| emacs | Yes | Yes |
| zsh | Yes | Yes |
| hypr | - | Yes |
| quickshell | - | Yes |
| nixos | - | Yes |

## License

MIT
