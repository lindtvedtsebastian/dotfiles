# prompt:
# %F => color dict
# %f => reset color
# %~ => current path
# %* => time
# %n => username
# %m => shortname host
# %(?..) => prompt conditional - %(condition.true.false)

# Single git status + branch info, parsed in one pass (no subshells)
git_prompt_info() {
    local line branch xy git_status=""
    local has_untracked has_added has_modified has_renamed has_deleted has_unmerged
    local has_ahead has_behind

    local porcelain
    porcelain=$(command git status --porcelain -b 2>/dev/null) || return

    while IFS= read -r line; do
        # Header line: "## main...origin/main [ahead 1, behind 2]"
        if [[ $line == '## '* ]]; then
            branch=${line#\#\# }
            branch=${branch%%...*}
            branch=${branch%% \[*}
            branch=${branch:0:10}
            [[ $line == *ahead* ]]  && has_ahead=1
            [[ $line == *behind* ]] && has_behind=1
            continue
        fi

        # XY status code: first two chars of each porcelain line
        xy=${line[1,2]}

        [[ $xy == '??' ]] && has_untracked=1
        [[ $xy == 'UU' ]] && has_unmerged=1
        [[ $xy[1] == [MARC] ]] && has_added=1
        [[ $xy[2] == 'M' || $xy[2] == 'T' ]] && has_modified=1
        [[ $xy[1] == 'R' ]] && has_renamed=1
        [[ $xy[1] == 'D' || $xy[2] == 'D' ]] && has_deleted=1
    done <<< "$porcelain"

    [[ -z $branch ]] && return

    # Stash detection (not available in porcelain output)
    local has_stash
    command git rev-parse --verify refs/stash &>/dev/null && has_stash=1

    [[ -n $has_untracked ]] && git_status+="%F{white}untrk%f "
    [[ -n $has_added ]]     && git_status+="%F{green}+%f "
    [[ -n $has_modified ]]  && git_status+="%F{blue}mod%f "
    [[ -n $has_renamed ]]   && git_status+="%F{magenta}rname%f "
    [[ -n $has_deleted ]]   && git_status+="%F{red}x%f "
    [[ -n $has_unmerged ]]  && git_status+="%F{yellow}═%f "
    [[ -n $has_stash ]]     && git_status+="%B%F{red}stsh%f%b "
    [[ -n $has_behind ]]    && git_status+="%B%F{red}bhnd%f%b "
    [[ -n $has_ahead ]]     && git_status+="%B%F{green}ahd%f%b "

    local result=" %F{red}λ%f:%F{white}${branch}%f"
    [[ -n $git_status ]] && result+=" [ ${git_status}]"
    echo "$result"
}

prompt_purity_precmd() {
    # Print a blank line before each prompt
    print -P ''
}

prompt_purification_setup() {
    autoload -Uz add-zsh-hook
    add-zsh-hook precmd prompt_purity_precmd

    setopt prompt_subst
    RPROMPT='$(git_prompt_info)'
    PROMPT=$'%F{white}%~ %B%F{blue}>%f%b '
}

prompt_purification_setup
