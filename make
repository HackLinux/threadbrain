#!/bin/bash

# set this to the path to the quartus bin directory
QUARTUS_PATH="../altera/14.0/quartus/bin"

rootjtag() {
	echo "Need to run jtagd as root..."
	sudo $QUARTUS_PATH/jtagd --user-start --config ~/.jtagd.conf
}

if [[ $# -lt 1 ]] ; then
	echo "Usage: $0 <project name>"
	echo "Make sure the current working directory has the project file."
	exit 1
fi

TARGET=$1
TMP=$(mktemp)

pgrep jtagd > $TMP
if [[ $? -ne 0 ]] ; then
	rootjtag
else
	LAST=$(tail -n 1 $TMP)
	ps u $LAST | grep "^root" > /dev/null
	if [[ $? -ne 0 ]] ; then
		pkill -9 jtagd
		rootjtag
	fi
fi

$QUARTUS_PATH/quartus_map --read_settings_files=on --write_settings_files=off $TARGET -c $TARGET
RET=$?
if [[ $RET -ne 0 ]] ; then
	exit $RET
fi
$QUARTUS_PATH/quartus_fit --read_settings_files=on --write_settings_files=off $TARGET -c $TARGET
$QUARTUS_PATH/quartus_asm --read_settings_files=on --write_settings_files=off $TARGET -c $TARGET
$QUARTUS_PATH/quartus_pgm $TARGET.cdf

