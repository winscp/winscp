#!/bin/bash -e
export LC_ALL=C
tmpfile=`mktemp`
nm -D $1 -p | sed -n '/ [TBD] /{s/.* [TBD] //;p;}' | sort > $tmpfile
if cmp test/symvers.txt $tmpfile; then
    echo PASS: exported symbols as expected
    rv=0
else
    echo FAIL: exported symbol mismatch
    diff -c1 test/symvers.txt $tmpfile || true
    rv=1
fi
rm $tmpfile
exit $rv
