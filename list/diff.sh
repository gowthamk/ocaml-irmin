#!/bin/bash

nrounds=10
for n in $(seq 10 10 150); 
do 
  out=$(./alice_monkey $nrounds $n)
  out_arr=($out)
  out_arr_len=${#out_arr[*]}
  treekb=${out_arr[$out_arr_len-1]}
  echo "Experiment $n complete"
  cp -R /tmp/repos/mlist2.git _mlist.git
  cd _mlist.git
  v=$(git log --pretty="%H")
  arr=($v)
  len=${#arr[*]}
  sum=0
  gzip_sum=0
  for ((i=0; i<len-1; i++))
  do
      f1=${arr[$i]}
      f2=${arr[$i+1]}
      diff=$(git diff $f1 $f2 --name-only | xargs stat -f %z | paste -sd+ - | bc)
      sum=$((sum+diff))
      $(git diff $f1 $f2 --name-only | xargs tar -czf _test.tar.gz)
      gzip_size=$(stat -f %z _test.tar.gz)
      rm _test.tar.gz 
      gzip_sum=$((gzip_sum+gzip_size))
  done
  diffkb=$(echo "scale=2; $sum/(($len-1)*1024)" | bc)
  gzipkb=$(echo "scale=2; $gzip_sum/(($len-1)*1024)" | bc)
# total=$(ls state/* | xargs stat -f %z | paste -sd+ - | bc)
# totalkb=$(echo "scale=2; $total/1024" | bc)
  echo "gzip/diff/total = $gzipkb/$diffkb/$treekb KB"
  cd ..
  rm -rf _mlist.git
  echo "$nrounds,$n,$gzipkb,$diffkb,$treekb" >> diff_results.csv
done

