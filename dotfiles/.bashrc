[[ $- != *i* ]] && return

alias ls="ls --color=always"
alias grep="grep --color=auto"

alias ll="ls -alF"
alias la="ls -A"
alias l="ls -CF"

alias tm='tmux new -A -s "$USER"'

source_emacs_dumb_term() {
    export PAGER="cat"
    export TERM="xterm-256color"
    man () { /usr/bin/man "$@" | col -bx ; }
    grep -q "nixos" /etc/issue && man () { /run/current-system/sw/bin/man "$@" | col -bx ; }
    watch() { while true ; do "$@" ; sleep 2;  echo ; done }
}
[[ "$TERM" = dumb ]] && [[ "$INSIDE_EMACS" ]] && source_emacs_dumb_term

export PS1="><> \${?#0}"
