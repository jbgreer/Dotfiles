# Only source this once.
if [ -n "$__HM_SESS_VARS_SOURCED" ]; then return; fi
export __HM_SESS_VARS_SOURCED=1

export EDITOR="nvim"
export FZF_DEFAULT_OPTS="--color bg:#1e1e2e,bg+:#313244,fg:#cdd6f4,fg+:#cdd6f4,header:#cba6f7,hl:#cba6f7,hl+:#cba6f7,info:#cba6f7,marker:#cba6f7,pointer:#cba6f7,prompt:#cba6f7,spinner:#f5e0dc"
export GLAMOUR_STYLE="/nix/store/0iar4yjvvyn45xpk52k81773x2383fq8-source/themes/catppuccin-mocha.json"
export LG_CONFIG_FILE="/nix/store/9b8pv3qydhysryypj0y3c0chzq2j44l1-source/themes-mergable/mocha/mauve.yml,/Users/jbgreer/.config/lazygit/config.yml"
export XDG_CACHE_HOME="/Users/jbgreer/.cache"
export XDG_CONFIG_HOME="/Users/jbgreer/.config"
export XDG_DATA_HOME="/Users/jbgreer/.local/share"
export XDG_STATE_HOME="/Users/jbgreer/.local/state"
export PATH="$PATH${PATH:+:}/run/current-system/sw/bin:$HOME/.nix-profile/bin"
