#!/usr/bin/env bash
if [ "$1" == "" ] ; then  
    sudo ping -c 512 -i 0.01 8.8.8.8
else
    sudo ping -c 512 -i 0.01 $1
fi
