# XDG paths for config files, etc
export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share

# configurations for zsh
export ZDOTDIR=$XDG_CONFIG_HOME/zsh

# Add local path
path=($HOME/bin $path)
#
# Default editor
export EDITOR="nvim"

# OS specific sourcing
case $KERNEL_NAME in
  'darwin')
    [ -f $ZDOTDIR/.config/.zshrc.macosx ] && . $ZDOTDIR/.config/.zshrc.macosx]
    ;;
esac

# completion directory
fpath=($HOME/.config/zsh $fpath)

# git 
zstyle ':completion:*:*:git:*' script $HOME/.config/zsh/git-completion.bash

# fuzzy finder
[ -f $HOME/.config/fzf/fzf.zsh ] && source $HOME/.config/fzf/fzf.zsh

# load completion filepath
autoload -Uz compinit
compinit

# alias
alias vim="nvim"
alias vi="nvim"

# 2023-06-18 jbgreer moved to .zlogin
eval "$(starship init zsh)"
