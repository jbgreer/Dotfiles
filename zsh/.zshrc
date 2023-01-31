
# Default editor
export EDITOR="nvim"

# XDG paths for config files, etc
export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share

# configurations for zsh
export ZDOTDIR=$HOME/.config/zsh

# Add local path
path=($HOME/bin $path)

# brew environment
eval "$(/opt/homebrew/bin/brew shellenv)"


# Add cargo to path
[ -f $HOME/.cargo/env ] && source $HOME/.cargo/env

# Add python3.10 stuff to path
path+=/opt/homebrew/opt/python@3.10/Frameworks/Python.framework/Versions/3.10/bin

# ruby environments
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# iTerm2 zsh integration
[ -f "${HOME}/.config/iterm2/.iterm2_shell_integration.zsh" ] && source "${HOME}/.config/iterm2/.iterm2_shell_integration.zsh"

# completion directory
fpath=($HOME/.config/zsh $fpath)

# git 
zstyle ':completion:*:*:git:*' script $HOME/.config/zsh/git-completion.bash

# brew zsh-completions
fpath=("$(brew --prefix)/share/zsh-completions" $fpath)

# fuzzy finder
[ -f $HOME/.config/fzf/fzf.zsh ] && source $HOME/.config/fzf/fzf.zsh

# load completion filepath
autoload -Uz compinit
compinit

eval "$(starship init zsh)"

# aliias
alias vim="nvim"
alias vi="nvim"
alias pg_start="launchctl load ~/Library/LaunchAgents"
alias pg_stop="launchctl unload ~/Library/LaunchAgents"

