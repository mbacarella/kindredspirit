#!/bin/bash

MY_IP=10.1.1.120/24
DEV=enp8s0

if ! (ip address show "$DEV" | grep -q "$MY_IP"); then
  echo "*** Interface $DEV not configured..."
  sudo ip address add "$MY_IP" dev "$DEV"
  echo "*** Added $MY_IP to $DEV"
else
  echo "*** Interface $DEV has good address $MY_IP."
fi

./build.sh && ./kindredspirit.native -sound-dev default -no-beat-detection
