#!/usr/bin/env bash

for i in $(seq 1 $1); do
    x=$RANDOM
    y=$RANDOM
    echo $x $y
    curl -X POST "localhost:8085/api?x=$x&y=$y" > /dev/null
done

curl -X GET "localhost:8085/api"

for i in $(seq 1 $1); do
    x=$RANDOM
    y=$RANDOM
    echo $x $y
    curl -X POST "localhost:8085/api?x=$x&y=$y" > /dev/null
done

curl -X GET "localhost:8085/api"
