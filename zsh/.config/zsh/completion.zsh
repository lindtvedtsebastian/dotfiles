  # Initialize autocomplete
  autoload -U compinit; compinit

  _comp_options+=(globdots) # With hidden files

  fpath=($ZDOTDIR $fpath)

  if [[ ! -d $ZDOTDIR/plugins/zsh-autosuggestions ]]                                               
      then
          echo "${RED}zsh-autosuggestions not found, cloning${NOCOLOR}"
          git clone "http://github.com/zsh-users/zsh-autosuggestions" $ZDOTDIR/plugins/zsh-autosuggestions
          echo "${GREEN}zsh-autosuggestions successfully cloned${NOCOLOR}"
  fi

  source $ZDOTDIR/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

