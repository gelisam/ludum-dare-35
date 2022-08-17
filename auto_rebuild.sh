#!/bin/bash
./rebuild.sh
fswatcher --throttle 200 --path src -- ./rebuild.sh
