# 2023-06-18 jbgreer prepend /usr/local/bin if present
[ -d '/usr/local/bin' ] && path=('/usr/local/bin' $path)

# 2023-06-18 jbgreer prepend $HOME/.local/bin
[ -d "$HOME/.local/bin" ] && path=("$HOME/.local/bin" $path)

# 2023-06-18 jbgreer prepend $HOME/bin
[ -d "$HOME/bin" ] && path=("$HOME/bin" $path)

# Rust
[ -f $"HOME/.cargo/env" ] && . "$HOME/.cargo/env"

# Deduplicate path entries
typeset -U PATH path
export PATH

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
case $OSTYPE in
  'darwin22.0')
    [ -f $HOME/.zshrc.macosx ] && source $HOME/.zshrc.macosx
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

# turn off bell
unsetopt BEEP

# prompt
#eval "$(starship init zsh)"
autoload -Uz vcs_info
precmd() { vcs_info }
zstyle ':vsc_info:git:*' formats '%b '
setopt PROMPT_SUBST
PROMPT='%F{green}%m%f %F{blue}%~%f %F{red}${vcs_info_msg_0_}%f$ '

