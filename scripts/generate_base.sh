#!/bin/bash

# Generate a set of parse trees for propositional.pl
#
# Usage:
#
# ./generate_base.sh <fragment>
#
# <fragment> is prop or veridical
#
# All results are to be stored in base_{fragment} directory

grammar=$1

if [ -d base_${grammar} ]; then
   echo "Clear up base directory"
   rm -rf base_${grammar}
fi

mkdir -p base_${grammar}

# function yield(){
# file=$1
# cat $file \
#   | sed -E 's/\(S //g' \
#   | sed -E 's/\(NP //g' \
#   | sed -E 's/\(PN //g' \
#   | sed -E 's/\(NOT //g' \
#   | sed -E 's/\(AND //g' \
#   | sed -E 's/\(CONN //g' \
#   | sed -E 's/\(UNARY //g' \
#   | sed -E 's/\(BINARY //g' \
#   | sed -E 's/\(AUX //g' \
#   | sed -E 's/\(VP //g' \
#   | sed -E 's/\(IV//g' \
#   | sed -E 's/\(TV//g' \
#   | sed -E 's/\(//g' \
#   | sed -E 's/\)//g' \
#   | sed -E 's/^[ ]*//g'
# }

echo "Processing recursion depth0..."
swipl -s cfg_${grammar}.pl -g "plain(0,1)" -t halt --quiet > base_${grammar}/rec0.scheme
echo "Processing recursion depth1..."
swipl -s cfg_${grammar}.pl -g "plain(1,1)" -t halt --quiet > base_${grammar}/rec1.scheme

# echo "Processing depth2..."
# time swipl -s cfg_${grammar}.pl -g "plain(2,1)" -t halt --quiet > base_${grammar}/depth2.psd
#echo "Processing depth3..."
#time swipl -s cfg_${grammar}.pl -g "gen(3,13)" -t halt --quiet > base_${grammar}/depth3.psd
#echo "Processing depth4..."
#time swipl -s cfg_${grammar}.pl -g "gen(4,15)" -t halt --quiet > base_${grammar}/depth4.psd

function count(){
word=$1
file=$2
num=`grep -c " ${word}" $file`
echo -e "${word}\t\t${num}"
}

function word_count(){
file=$1
echo "######################"
wc -l ${file}
echo "# Propositional Connectives"
count "and" $file
count "or" $file
count "not" $file
}

word_count base_${grammar}/rec0.scheme
word_count base_${grammar}/rec1.scheme
# word_count base_${grammar}/depth2.scheme
#word_count base_${grammar}/depth3.txt
#word_count base_${grammar}/depth4.txt
