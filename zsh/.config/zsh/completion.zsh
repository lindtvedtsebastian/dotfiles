  # Initialize autocomplete
  autoload -U compinit; compinit

  _comp_options+=(globdots) # With hidden files

  fpath=($ZDOTDIR $fpath)

  if [[ ! -d $ZDOTDIR/plugins/zsh-autosuggestions ]]
      then
          print -P "%F{red}zsh-autosuggestions not found, cloning%f"
          git clone "https://github.com/zsh-users/zsh-autosuggestions" $ZDOTDIR/plugins/zsh-autosuggestions
          print -P "%F{green}zsh-autosuggestions successfully cloned%f"
  fi

  source $ZDOTDIR/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

  if [[ ! -d $ZDOTDIR/plugins/zsh-syntax-highlighting ]]
      then
          print -P "%F{red}zsh-syntax-highlighting not found, cloning%f"
          git clone "https://github.com/zsh-users/zsh-syntax-highlighting" $ZDOTDIR/plugins/zsh-syntax-highlighting
          print -P "%F{green}zsh-syntax-highlighting successfully cloned%f"
  fi

  # Must be sourced last, after all ZLE widgets are defined
  source $ZDOTDIR/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

