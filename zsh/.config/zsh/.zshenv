# Environment variables
. "/etc/profiles/per-user/jbgreer/etc/profile.d/hm-session-vars.sh"

# Only source this once
if [[ -z "$__HM_ZSH_SESS_VARS_SOURCED" ]]; then
  export __HM_ZSH_SESS_VARS_SOURCED=1
  export EDITOR="nvim"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_RUNTIME_DIR="$HOME/.xdg"
export XDG_STATE_HOME="$HOME/.local/state"
fi

export ZDOTDIR=$HOME/.config/zsh

# programs.zsh.envExtra

# Racket
[ -d '/Applications/Racket v8.14/bin' ] && path+=('/Applications/Racket v8.14/bin')

# programs.zsh.envExtra
