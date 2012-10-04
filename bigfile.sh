#!/bin/sh
# Copyright © 2012 Bart Massey
# [This program is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.

# Write n copies of the file argument to stdout.

COUNT=$1; shift
FILE=$1; shift

while [ $COUNT -gt 0 ]
do
    cat "$FILE"
    COUNT=`expr $COUNT - 1`
done
exit 0
