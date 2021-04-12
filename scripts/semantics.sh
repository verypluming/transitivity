#!/bin/bash

# Usage:
# <frag> is prop or veridical
# eg.
# ./semantics prop

frag=$1

dir="results_${frag}"
semantics="cfg_${frag}_pn.pl"

IFS=$'\n'

function str_to_list(){
 str=$1
 cat $str \
   | sed -e 's/ $//g' \
   | sed 's/ /,/g' \
   | sed -e 's/^/[/g' \
   | sed -e 's/$/]/g'
}

for file in ${dir}/depth*; do
  # basename=${file##*/}
  # depth=$(echo $basename | cut -b 6)
  str_to_list $file | \
  while read line; do
    swipl -s ${semantics} -g "semparse(${line})" -t halt --quiet
  done
done
