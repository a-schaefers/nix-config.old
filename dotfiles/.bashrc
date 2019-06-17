[[ $- != *i* ]] && return

export PS1="> \${?#0}"

alias ls="ls --color=always"
alias grep="grep --color=auto"

alias ll="ls -alF"
alias la="ls -A"
alias l="ls -CF"

emacs_dumb_term() {
    export PAGER="cat"
    export TERM="xterm-256color"
    man () { /usr/bin/man "$@" | col -bx ; }
    grep -q "nixos" /etc/issue && man () { /run/current-system/sw/bin/man "$@" | col -bx ; }
    watch() { while true ; do "$@" ; sleep 2;  echo ; done }
}
[[ "$TERM" = dumb ]] && [[ "$INSIDE_EMACS" ]] && emacs_dumb_term

extract() {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)      tar xjf $1                                      ;;
            *.tar.gz)       tar xzf $1                                      ;;
            *.bz2)          bunzip2 $1                                      ;;
            *.rar)          rar x $1                                        ;;
            *.gz)           gunzip $1                                       ;;
            *.tar)          tar xf $1                                       ;;
            *.xz)           tar xf $1                                       ;;
            *.tbz2)         tar xjf $1                                      ;;
            *.tgz)          tar xzf $1                                      ;;
            *.zip)          unzip $1                                        ;;
            *.Z)            uncompress $1                                   ;;
            *)              echo "'$1' cannot be extracted via extract()"   ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}
