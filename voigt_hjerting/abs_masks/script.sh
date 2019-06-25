#!/bin/bash

z=0.0
for z in $(seq  12.001 0.5  20.001) ;
do
./profiles 1 $1 $z > synthetic1_${z}_${1}dat
done
