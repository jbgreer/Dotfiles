#  set -x

# get rid of WSL cruft
export PATH='/usr/sbin:/usr/bin:/sbin:/bin'

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

# Default editor
bindkey -v
export EDITOR="nvim"

# OS specific sourcing
case $OSTYPE in
  'darwin22.0')
    [ -f $ZDOTDIR/.zshrc.macosx ] && source $ZDOTDIR/.zshrc.macosx
    ;;
  'linux-gnu')
    [ -f $ZDOTDIR/.zshrc.wsl ] && source $ZDOTDIR/.zshrc.wsl
    ;;
esac

# completion directory
fpath=($ZDOTDIR/zsh $fpath)

# git 
zstyle ':completion:*:*:git:*' script $ZDOTDIR/git-completion.bash

# fuzzy finder
#[ -f $XDG_CONFIG_HOME/fzf/fzf.zsh ] && source $XDG_CONFIG_HOME/fzf/fzf.zsh

# load completion filepath
autoload -Uz compinit
compinit

# alias
alias e="emacsclient -c -a emacs"
alias vim="nvim"
alias vi="nvim"

# turn off bell
unsetopt BEEP

# ssh-agent
if [ $(ps ax | grep "[s]sh-agent" | wc -l) -eq 0 ] ; then
  eval $(ssh-agent -s) > /dev/null
  if [ "$(ssh-add -l)" = "The agent has no identities." ] ; then
    ssh-add ~/id_ed25519 > /dev/null 2>&1
  fi
fi
# prompt
#eval "$(starship init zsh)"
#autoload -Uz vcs_info
#precmd() { vcs_info }
#zstyle ':vsc_info:git:*' formats '%b '
setopt PROMPT_SUBST
PROMPT='%F{green}%m%f %F{blue}%~%f %F{red}${vcs_info_msg_0_}%f$ '

