#!/bin/sh

# intention: make pwd something specific
cd /tmp/

umask 0002

if [ ! -e "${DWIM_EXECUTABLE_CORE_FILE}" ]; then
  echo
  echo "ERROR: '${DWIM_EXECUTABLE_CORE_FILE}' doesn't exist, try bin/build.sh!"
  exit -1
fi

echo
echo "*** Starting ${DWIM_PROJECT_NAME} from ${DWIM_EXECUTABLE_CORE_FILE}"
echo

echo `date` - server loop started >>${DWIM_LOG_DIRECTORY}/start.log

# this doesn't work as expected... trap "echo \`date\` - server loop exiting >>${DWIM_LOG_DIRECTORY}/start.log" exit

while true; do

  # TODO reinstate --dynamic-space-size and --lose-on-corruption. original was: ${DWIM_EXECUTABLE_CORE_FILE} --dynamic-space-size ${DYNAMIC_SPACE_SIZE} --lose-on-corruption --end-runtime-options --no-userinit --no-sysinit --end-toplevel-options >>/var/log/${DWIM_PROJECT_NAME}/standard-output.log 2>&1 $*
  ${DWIM_EXECUTABLE_CORE_FILE} >>/var/log/${DWIM_PROJECT_NAME}/standard-output.log 2>&1 $*
  if [ "$?" -ne "0" ]; then
    echo `date` - abnormal exit with code $?, restarting >>${DWIM_LOG_DIRECTORY}/start.log
    # TODO start sleeping from 1 sec and make it grow with each restart until an upper bound
    sleep 5;
  else
    echo `date` - normal exit, restarting >>${DWIM_LOG_DIRECTORY}/start.log
    sleep 1;
  fi

done
