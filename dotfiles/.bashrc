[[ $- != *i* ]] && return

alias ls="ls --color=always"
alias grep="grep --color=auto"

alias ll="ls -alF"
alias la="ls -A"
alias l="ls -CF"

alias tm='tmux new -A -s "$USER"'

wget_crawler() { wget --no-clobber --convert-links --random-wait -r -p --level 1 -E -e robots=off -U mozilla "$@"; }

keychain_unlock() {
    keychain -q --gpg2 --agents ssh,gpg --eval id_rsa 59AF55B230F3A044AF17DB6D09C5261E6305B722
    pgrep emacs && emacsclient -e "(keychain-refresh-environment)"
}

keychain_lock() { keychain -k all --agents ssh,gpg ; }

source_emacs_dumb_term() {
    export PAGER="cat"
    export TERM="xterm-256color"
    man () { /usr/bin/man "$@" | col -bx ; }
    grep -q "nixos" /etc/issue && man () { /run/current-system/sw/bin/man "$@" | col -bx ; }
    watch() { while true ; do "$@" ; sleep 2;  echo ; done }
}
[[ "$TERM" = dumb ]] && [[ "$INSIDE_EMACS" ]] && source_emacs_dumb_term

export PS1="> \${?#0}"
