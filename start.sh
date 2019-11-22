#!/bin/bash

EXE="kindredspirit.native"
MY_IP=10.1.1.120/24
DEV=enp8s0

if ! (/bin/ip address show "$DEV" | grep -q "$MY_IP"); then
  echo "*** Interface $DEV not configured..."
  sudo /bin/ip address add "$MY_IP" dev "$DEV"
  echo "*** Added $MY_IP to $DEV"
else
  echo "*** Interface $DEV has good address $MY_IP."
fi

echo "*** Now starting $EXE..."
#~/stop-dhcp.sh
#./build.sh &&
dune exec ./kindredspirit.exe -- -config-file configs/thinkpad_sl410.sexp
