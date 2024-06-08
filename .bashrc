#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias lsh='ls -lahtr'
alias grep='grep --color=auto'
alias ssh='TERM=xterm-256color ssh'
PS1='[\u@\h \W]\$ '
