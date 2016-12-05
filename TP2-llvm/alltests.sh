#!/bin/bash

for f in tests/testlevel2/*.vsl
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
# tableaux
# probleme de scope : testleve4t1.vsl
# boucle infinie : testlevel4testcarre.vsl
