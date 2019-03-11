#!/bin/bash

nrounds=10
for i in $(seq 80 10 150); 
do 
  ./monkey $nrounds $i
  echo "Experiment $i complete once"
  ./monkey $nrounds $i
  echo "Experiment $i complete twice"
  ./monkey $nrounds $i
  echo "Experiment $i complete thrice"
done
