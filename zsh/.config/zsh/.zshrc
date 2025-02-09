
# programs.zsh.initExtraFirst
# programs.zsh.initExtraFirst

typeset -U path cdpath fpath manpath

for profile in ${(z)NIX_PROFILES}; do
  fpath+=($profile/share/zsh/site-functions $profile/share/zsh/$ZSH_VERSION/functions $profile/share/zsh/vendor-completions)
done

HELPDIR="/nix/store/l9jyhclsyss3fj81a83p86ccqm416dms-zsh-5.9/share/zsh/$ZSH_VERSION/help"

# Use viins keymap as the default.
bindkey -v


# programs.zsh.initExtraBeforeCompInit
# programs.zsh.initExtraBeforeCompInit


# Oh-My-Zsh/Prezto calls compinit during initialization,
# calling it twice causes slight start up slowdown
# as all $fpath entries will be traversed again.
autoload -U compinit && compinit
source /nix/store/8qaglv27dwsdnrv2lrhxja9pc9162sz6-zsh-autosuggestions-0.7.1/share/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_AUTOSUGGEST_STRATEGY=(history)








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


if [[ $options[zle] = on ]]; then
  eval "$(/nix/store/x2fwnan6h8r8z048i0lrakqzf078j5gn-fzf-0.58.0/bin/fzf --zsh)"
fi

source '/nix/store/d7cb446fcq4karwhr935mhgwfa8b0h64-zsh-syntax-highlighting-7926c3d/catppuccin_mocha-zsh-syntax-highlighting.zsh'

# programs.zsh.initExtra
# programs.zsh.initExtra

eval "$(/nix/store/zn1p72pzd1fcswv76qzw7kn7y81p5iak-oh-my-posh-24.11.4/bin/oh-my-posh init zsh --config /nix/store/zn1p72pzd1fcswv76qzw7kn7y81p5iak-oh-my-posh-24.11.4/share/oh-my-posh/themes/catppuccin_mocha.omp.json)"

eval "$(/nix/store/q00mwz61l0kz3ikjafm8v42df79vw70m-direnv-2.35.0/bin/direnv hook zsh)"


# Aliases
alias -- eza='eza --git'
alias -- l='ls -al'
alias -- la='eza -a'
alias -- ll='eza -l'
alias -- lla='eza -la'
alias -- ls=eza
alias -- lt='eza --tree'
alias -- vim=nvim

# Named Directory Hashes




