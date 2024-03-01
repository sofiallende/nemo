#!/bin/bash
#
# this sed makes sure that we alwas specify the _wp in the calls to lbc_lnk
#
for ff in $( find src -type f ) */*/MY_SRC/*  # for all nemo files
do
    # look for something like ", 'X', 1. )" and replace it by ", 'X', 1.wp )"
    #   - X can be TUVFW
    #   - the "1" can be followed by a "." or not. It must be followed by "," or ")" or "&"
    #   - any number of blanks is acceptable.
    sed -e "s/\(, *'[TUVFW]' *, *-*1\)\.*\( *[,)&]\)/\1._wp\2/g" $ff > tmp
    mv tmp $ff
done

  
