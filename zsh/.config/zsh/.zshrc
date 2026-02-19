typeset -U path cdpath fpath manpath

# Use viins keymap as the default.
bindkey -v

# History options 
export HISTSIZE=10000
export SAVEHIST=$HISTSIZE
HISTFILE="$HOME/.zsh_history"
mkdir -p "$(dirname "$HISTFILE")"

setopt HIST_FCNTL_LOCK
unsetopt APPEND_HISTORY
setopt HIST_IGNORE_DUPS
unsetopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
unsetopt HIST_EXPIRE_DUPS_FIRST
setopt SHARE_HISTORY
setopt EXTENDED_HISTORY

# Prevent forwarded SSH agent from leaking into shell sessions
unset SSH_AUTH_SOCK

autoload -Uz compinit && compinit

# FZF 
# https://github.com/junegunn/fzf#setting-up-shell-integration
source <(fzf --zsh)

# starship prompt setup
eval "$(starship init zsh)"

