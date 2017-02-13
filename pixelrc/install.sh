#!/bin/bash

set -x

if [ "$1" = "" ]; then
  echo "usage: $0 <pixel-pusher-id>"
  exit 1
fi

sudo mount /dev/sdb1 /mnt/usb-disk/ \
&& sleep 2 \
&& sudo cp $1/pixel.rc /mnt/usb-disk \
&& sudo umount /mnt/usb-disk
