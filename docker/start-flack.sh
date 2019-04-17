#!/bin/bash

if [ "$FLACK_CLUSTER" != "" ]; then
    sed -i "s/all/\[$FLACK_CLUSTER\]/g" /root/releases/1.0.2/sys.config
fi

exec /root/bin/flack foreground
