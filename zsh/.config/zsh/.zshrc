typeset -U path cdpath fpath manpath

# ZSH autocomplete
#source /opt/homebrew/share/zsh-autocomplete/zsh-autocomplete.plugin.zsh

# Use viins keymap as the default.
bindkey -v

# History options should be set in .zshrc and after oh-my-zsh sourcing.
# See https://github.com/nix-community/home-manager/issues/177.
HISTSIZE="10000"
SAVEHIST="10000"

HISTFILE="$HOME/.zsh_history"
mkdir -p "$(dirname "$HISTFILE")"

setopt HIST_FCNTL_LOCK
unsetopt APPEND_HISTORY
setopt HIST_IGNORE_DUPS
unsetopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
unsetopt HIST_EXPIRE_DUPS_FIRST
setopt SHARE_HISTORY
unsetopt EXTENDED_HISTORY


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

# OH-MY-POSH
eval "$(oh-my-posh init zsh --config $(brew --prefix oh-my-posh)/themes/catppuccin.omp.json)"

# RUST 
[ -d $HOME/.cargo ] && path+=$HOME/.cargo/bin

# uv environments
[ -d $HOME/.local/bin ] && path=("$HOME/.local/bin" $path)

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

# Named Directory Hashes
