#!/bin/bash
./rebuild.sh
fswatch --one-per-batch *.elm | while read X; do ./rebuild.sh; done
