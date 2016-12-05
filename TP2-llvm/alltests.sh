#!/bin/bash

for f in tests/*/*.vsl
do
    printf "\n\n------------------------------------------------\n\n"
    echo $f
    cat $f
    echo
    nf=${f%.vsl}
    ./compile.sh $nf
    ./$nf
done

yes | rm tests/*/*.o
yes | rm tests/*.o
