#!/bin/bash

nrounds=10
for i in $(seq 10 10 150); 
do 
  ./monkey $nrounds $i
  echo "Experiment $i completed once"
  ./monkey $nrounds $i
  echo "Experiment $i completed twice"
  ./monkey $nrounds $i
  echo "Experiment $i completed thrice"
done
