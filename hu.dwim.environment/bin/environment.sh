#!/bin/bash
# -*- mode: shell-script; sh-basic-offset: 2; indent-tabs-mode: nil -*-

#alias mysql='mysql --user=root --password=admin123 --database=dwim'
alias tt='tar -zvtf'
alias tx='tar -zvxf'
alias tc='tar -zvcf'
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias ll='ls -l'
alias la='ls -A'

if [ -z "$DWIM_WORKSPACE" ] ; then
  export DWIM_WORKSPACE=~/workspace
fi

# if the workspace doesn't exist, we're in trouble
function check_workspace () {
  if [[ -z $DWIM_WORKSPACE || ! -e $DWIM_WORKSPACE ]] ; then
    printf "\nThe dwim workspace '%s' does not exist.
Either create a symbolic link at that location pointing to the location where you
checked out dwim, or set the env var DWIM_WORKSPACE correctly\n\n" $DWIM_WORKSPACE
    return 1
  fi
  return 0
}

check_workspace

export DARCS_DONT_ESCAPE_8BIT=1
alias wnew='darcs w -l'

case $USER
in
   ati)
      export REMOTE_USER_NAME="alendvai"
   ;;
   levy)
      export REMOTE_USER_NAME="lmeszaros"
   ;;
   tomi)
      export REMOTE_USER_NAME="tborbely"
   ;;
esac

function rmfasl ()              # public completion:dir
{
  for i in $*; do
    for dir in `find ~/.cache/common-lisp/ /opt/*/.cache/common-lisp/ -type d -wholename "/*/*/*/*/*$i*" 2>/dev/null`; do
      if [ -e "$dir" ]; then
        echo Deleting "$dir"
        rm -r "$dir"
      fi
    done
  done
}

function revertall ()           # public completion:dir
{
  for dir in $*; do
    if [ -d $dir ]; then
      echo -n "Reverting: ["$dir"] .. "
      pushd $dir >/dev/null
      if [ -d "_darcs" ]; then
        echo "using [darcs]."
        darcs revert
      else
        echo "No source control system found."
      fi
      popd >/dev/null
    else
      echo No such dir, skipping \"$dir\"!
    fi
  done
}

function pullallrepos ()        # public completion:repo
{
  for dir in $*; do
    if [ -d $dir ]; then
      echo -n "Update: ["$dir"] .. "
      pushd $dir >/dev/null
      if [ -d "_darcs" ]; then
        echo "using [darcs]."
        darcs pull
      elif [ -d ".svn" ]; then
        echo "using [svn]."
        svn up
      elif [ -d "CVS" ]; then
        echo "using [cvs]."
        cvs up -d
      elif [ -d ".git" ]; then
        echo "using [git]."
        git pull
      else
        echo "No source control system found."
      fi
      popd >/dev/null
    else
      echo No such dir, skipping \"$dir\"!
    fi
  done
}

function pushallrepos ()        # public completion:dir
{
  for dir in $*; do
    if [ -d $dir ]; then
      echo -n "Pushing: ["$dir"] .. "
      pushd "$dir" >/dev/null
      darcs push
      popd >/dev/null
    else
      echo No such dir, skipping \"$dir\"!
    fi
  done
}

function pulldwimrepos ()       # public completion:dir
{
  for dir in hu.dwim.*; do
    echo "Pulling in ["$dir"]"
    pushd "$dir" >/dev/null
    if [ $# = 1 ]; then
      for arg in "$*"; do
        last=$arg
        processed_args="$processed_args $arg"
      done
      echo $ darcs pull $1$dir
      darcs pull $1$dir
    else
        if [ $# = 0 ]; then
          darcs pull
        else
          echo "Syntax error for pulldwimrepos"
          exit 1
        fi
    fi
    echo
    popd >/dev/null
  done
}

function pullknowledgetoolsrepos ()       # public completion:dir
{
  for dir in hu.dwim.asdf hu.dwim.def hu.dwim.delico hu.dwim.logger hu.dwim.rdbms hu.dwim.perec hu.dwim.quasi-quote hu.dwim.serializer hu.dwim.stefil hu.dwim.util hu.dwim.walker ; do
    echo "Pulling in ["$dir"]"
    pushd "$dir" >/dev/null
    darcs pull --no-set-default http://www.knowledgetools.de/tmp/temporaer/tomas/$dir
    echo
    popd >/dev/null
  done
}

function dodwimrepos ()         # public completion:dir
{
  # example usage (set the repo url for all the dwim projects):
  # dodwimrepos darcs pull --set-default http://dwim.hu/darcs/\$dir
  echo "Will execute the following in all hu.dwim repos:"
  echo "$ $*"
  echo
  sleep 1
  # echo "Sure? Press enter to continue..."
  # read
  for dir in hu.dwim.*; do
    echo "Visiting ["$dir"]"
    pushd "$dir" >/dev/null
    sh -c "dir=$dir ; export dir ; $*"
    popd >/dev/null
  done
}

function pullstuff ()           # public completion:none
{
   pullallrepos alexandria \
       anaphora \
       asdf-binary-locations \
       asdf-system-connections \
       babel \
       bordeaux-threads \
       cffi \
       chunga \
       cl+ssl \
       cl-base64 \
       cl-containers \
       cl-fad \
       cl-graph \
       cl-l10n \
       cl-pdf \
       cl-ppcre \
       cl-smtp \
       cl-typesetting \
       cl-yacc \
       closer-mop \
       closure-common \
       command-line-arguments \
       contextl \
       cxml \
       darcsweb \
       dojo \
       drakma \
       flexi-streams \
       ieee-floats \
       iolib \
       ironclad \
       iterate \
       local-time \
       lw-compat \
       md5 \
       metabang-bind \
       metacopy \
       metatilities \
       metatilities-base \
       moptilities \
       osicat \
       parse-number \
       plexippus-xpath \
       postmodern \
       puri \
       rfc2109 \
       rfc2388-binary \
       split-sequence \
       trivial-features \
       trivial-garbage \
       trivial-gray-streams \
       trivial-shell \
       trivial-utf-8 \
       usocket \
       verrazano

   pulldwimrepos
}

function pushdwimrepos ()       # public completion:none
{
  pushallrepos hu.dwim.*
}

function lfind                  # public completion:none
{
  grep -i -n $1 `find $2 -name "*.lisp" | grep -v _darcs`
}

function elfind                 # public completion:none
{
  grep -i -n $1 `find $2 -name "*.el" | grep -v _darcs`
}

if [ -d $DWIM_WORKSPACE/hu.dwim.environment/bin ]; then
  export PATH=$DWIM_WORKSPACE/hu.dwim.environment/bin:$PATH
fi

if [ -d $DWIM_WORKSPACE/hu.dwim.build/bin ]; then
  export PATH=$DWIM_WORKSPACE/hu.dwim.build/bin:$PATH
fi
