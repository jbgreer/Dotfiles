# 
HISTFILE=$ZDOTDIR/.histfile
HISTSIZE=1000
SAVEHIST=1000

# augment path with local directories
typeset -U path PATH
path=(~/bin $path)
path=(~/.local/bin $path)
export PATH

# prefer vi command-line editing and set editor
bindkey -v
export EDITOR="nvim"

zstyle ':completion:*' completer _complete _ignored
zstyle :compinstall filename '/home/jbgreer/.zshrc'

# completions and prompt support
fpath=($ZDOTDIR $fpath)
autoload -Uz compinit
compinit

# aliases
alias vi="nvim"

# turn off bell
unsetopt BEEP

# ssh-agent
export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/ssh-agent.socket

# mise-en-place
eval "$(/usr/bin/mise activate zsh)"

# fzf
[ -f /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh
[ -f /usr/share/fzf/completion.zsh ] && source /usr/share/fzf/completion.zsh
export FZF_DEFAULT_OPTS=" \
--color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
--color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
--color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"

# git
zstyle ':completion:*:*:git:*' script $ZDOTDIR/.git-completion.bash

# starship, but only in graphical mode
[ -n "$DISPLAY" ] && [ "$XDG_VTNR" -eq 1 ] && eval "$(starship init zsh)"

# Hyprland, but only on tty1
[ -z "$DISPLAY" ] && [ "$XDG_VTNR" -eq 1 ] && [ $(tty) = "/dev/tty1" ] && exec Hyprland

