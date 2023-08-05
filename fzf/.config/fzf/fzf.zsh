# Setup fzf
# ---------
if [[ ! "$PATH" == */opt/homebrew/opt/fzf/bin* ]]; then
  PATH="${PATH:+${PATH}:}/opt/homebrew/opt/fzf/bin"
fi

# Auto-completion and keybindings
# ---------------
# MacOS
if [[ $- == *i* ]]; then
  [ -f "/opt/homebrew/opt/fzf/shell/completion.zsh" ] && source "/opt/homebrew/opt/fzf/shell/completion.zsh" 2> /dev/null
  [ -f "/opt/homebrew/opt/fzf/shell/key-bindings.zsh" ] && source "/opt/homebrew/opt/fzf/shell/key-bindings.zsh"
fi

# Ubuntu
if [[ $- == *i* ]]; then
  [ -f "/usr/share/doc/fzf/examples/completion.zsh" ] && source "/usr/share/doc/fzf/examples/completion.zsh" 2> /dev/null
  2> /dev/null
  [ -f "/opt/homebrew/opt/fzf/shell/key-bindings.zsh" ] && source "/opt/homebrew/opt/fzf/shell/key-bindings.zsh"
fi

