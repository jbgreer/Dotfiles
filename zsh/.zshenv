# 2023-06-18 jbgreer reset path to basics
PATH='/usr/sbin:/usr/bin:/sbin:/bin'

# 2023-06-18 jbgreer prepend /usr/local/bin if present
[ -d '/usr/local/bin' ] && path=('/usr/local/bin' $path)

# 2023-06-18 jbgreer prepend $HOME/.local/bin
[ -d "$HOME/.local/bin" ] && path=("$HOME/.local/bin" $path)

# 2023-06-18 jbgreer prepend $HOME/bin
[ -d "$HOME/bin" ] && path=("$HOME/bin" $path)

# Rust
[ -f $"HOME/.cargo/env" ] && . "$HOME/.cargo/env"

# Deduplicate path entries
#typeset -U PATH path
export PATH
