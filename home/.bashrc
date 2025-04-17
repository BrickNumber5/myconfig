#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Setup basic aliases
alias ls='ls --color=auto'
alias lsh='ls -lahtr'
alias grep='grep --color=auto'
alias ssh='TERM=xterm-256color ssh'

# Set common environment variables
export EDITOR=/bin/vim

# Set prompt
. ~/.ps1.sh
PS1='$(__bri_ps1__render)'
PS2='$(__bri_ps1__render_ps2)'
