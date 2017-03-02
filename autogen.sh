#!/bin/bash

fpcBaseInput=$1

if [ "$fpcBaseInput" == "" ]; then
  fpcBase=$(dirname $(dirname $(which fpc)))/lib/fpc/$(fpc -iV)
else
  fpcBase=$fpcBaseInput
fi

if [ ! -d $fpcBase ]; then
  echo "Need an FPC base to fpc sources"
  echo "Usually /usr/lib{32,64}/fpc/{fpc-version}/ on Linux machines."
  echo "or C:\FPC\..\.. on Windows machines."
  exit 255
fi

[ -f "Makefile" ] && rm Makefile

echo "Making with unit/source directory '$fpcBase'"

FPCDIR="$fpcBase" fpcmake

