alias ls='ls --color=auto'
alias ll='ls -l'
alias la='ls -a'

alias g='git'

parse_git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

export PS1="\e[0;32m[\w\[\033[33m\]\$(parse_git_branch)\[\033[00m\]\e[0;32m]\033[00m "
