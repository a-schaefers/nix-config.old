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

export PS1="> \${?#0}"

if [ "$USER" = "root" ]; then
    countdown()
    (
        IFS=:
        set -- $*
        secs=$(( ${1#0} * 3600 + ${2#0} * 60 + ${3#0} ))
        while [ $secs -gt 0 ]
        do
            sleep 1 &
            printf "\r%02d:%02d:%02d" $((secs/3600)) $(( (secs/60)%60)) $((secs%60))
            secs=$(( $secs - 1 ))
            wait
        done
        echo
    )
    if [ "$(tty)" = "/dev/tty1" ]; then
        echo "Starting graphical in..." ; countdown "00:00:03"
        systemctl restart display-manager
    fi
    export PS1="% \${?#0}"
fi
