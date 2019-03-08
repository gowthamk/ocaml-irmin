#!/bin/sh

for i in $(seq 100 100 500); 
do 
  ./monkey 50 $i
  echo "Experiment $i complete"
done
