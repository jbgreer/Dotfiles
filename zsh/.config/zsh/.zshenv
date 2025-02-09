# Environment variables

# Only source this once
if [[ -z "$__ZSH_SESS_VARS_SOURCED" ]]; then
  export __ZSH_SESS_VARS_SOURCED=1
  export EDITOR="nvim"
  export XDG_CACHE_HOME="$HOME/.cache"
  export XDG_CONFIG_HOME="$HOME/.config"
  export XDG_DATA_HOME="$HOME/.local/share"
  export XDG_RUNTIME_DIR="$HOME/.xdg"
  export XDG_STATE_HOME="$HOME/.local/state"

  export ZDOTDIR=$HOME/.config/zsh
fi

