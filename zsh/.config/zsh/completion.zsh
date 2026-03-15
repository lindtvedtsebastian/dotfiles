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

