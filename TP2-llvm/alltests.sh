#!/bin/bash

for f in tests/testlevel3/*.vsl
do
    printf "\n\n------------------------------------------------\n\n"
    echo $f
    cat $f
    echo
    nf=${f%.vsl}
    ./compile.sh $nf
    ./$nf
done

# calling void function (see testlevel2/level2expr.vsl)

