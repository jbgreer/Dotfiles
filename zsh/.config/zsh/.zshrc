# Uncomment for zsh profiling
# zmodload zsh/zprof 


# Use emacs bindings
bindkey -e

# history settings
export HISTSIZE=20000
export SAVEHIST=$HISTSIZE

HISTFILE="$HOME/.zsh_history"
[ ! -f $HISTFILE ] && mkdir -p "$(dirname "$HISTFILE")"

setopt HIST_FCNTL_LOCK
unsetopt APPEND_HISTORY
setopt HIST_IGNORE_DUPS
unsetopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
unsetopt HIST_EXPIRE_DUPS_FIRST
setopt SHARE_HISTORY
setopt EXTENDED_HISTORY

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

# JAVA_HOME for Clojure / Sci cloj Noj v2 setup
export JAVA_HOME=$(brew --prefix openjdk)/libexec/openjdk.jdk/Contents/Home

# RUST 
[ -d $HOME/.cargo ] && path+=$HOME/.cargo/bin

# Mise-en-place
eval "$(/opt/homebrew/bin/mise activate zsh)"

# prefer local bins
[ -d $HOME/.local/bin ] && path=("$HOME/.local/bin" $path)

# deduplicate path, cdpath, fpath, and manpath, keeping only 1st instance
typeset -U path cdpath fpath manpath

# Aliases
alias -- eza='eza --git'
alias -- l='ls -al'
alias -- la='eza -a'
alias -- ll='eza -l'
alias -- lla='eza -la'
alias -- ls=eza
alias -- lt='eza --tree'
alias -- vim="nvim"
alias -- e="emacsclient -c -a emacs"

# Uncomment for zsh profiling
# zprof
