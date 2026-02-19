typeset -U path cdpath fpath manpath

# ZSH autocomplete
#source /opt/homebrew/share/zsh-autocomplete/zsh-autocomplete.plugin.zsh

# Use viins keymap as the default.
bindkey -v

# History options should be set in .zshrc and after oh-my-zsh sourcing.
# See https://github.com/nix-community/home-manager/issues/177.
# 2026-01-18 jbgreer exporting HISTSIZE and SAVEHIST
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
# 2026-01-18 jbgreer now setting EXTENDED_HISTORY; adds timestamps 
setopt EXTENDED_HISTORY

# Prevent forwarded SSH agent from leaking into shell sessions
unset SSH_AUTH_SOCK

# ZSH autosuggestions
#source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh

# ZSH completions
if type brew &>/dev/null; then
  fpath+=/opt/homebrew/share/zsh-completions
  fpath+=/opt/homebrew/share/zsh/site-functions
fi
autoload -Uz compinit && compinit

# FZF
# https://github.com/junegunn/fzf#setting-up-shell-integration
# set up fzf key bindings and fuzzy completion
source <(fzf --zsh)

# starship
# 2026-01-18 jbgreer adding
eval "$(starship init zsh)"

