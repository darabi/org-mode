export EMAIL="levente.meszaros@gmail.com"
export RSYNC_RSH=ssh

. ~/workspace/hu.dwim.environment/bin/environment.sh

#export PATH=/usr/local/Gambit-C/bin:$PATH
export PATH=~/.cabal/bin:$PATH

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups
export HISTFILESIZE=5000
export HISTSIZE=5000
export EDITOR=gedit
export LESS="--chop-long-lines --RAW-CONTROL-CHARS" # --quit-if-one-screen

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias ll='ls -la'
alias la='ls -A'
