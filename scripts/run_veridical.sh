#!/bin/bash

# Script to generate veridical inferences

# Set the number of sentences generated for each depth
total=$1

if [ -d base_veridical ]; then
   echo "Clear up base directory"
   rm -rf base_veridical
fi

if [ -d results_veridical ]; then
   echo "Clear up results directory"
   rm -rf results_veridical
fi

echo "Generate sentence schemas"
echo "Running generate_base.sh"
./generate_base.sh veridical

echo "Estimating the number of sentence generated"
echo "Running analyze.sh"
# Set the number of proper names as the first argument
./analyze_veridical.sh 10 $total

echo "Instantiate sentence schema with pruning rate"
echo "Running instantiate.py"
python instantiate_veridical.py $total

if [ -f results_veridical/all_formulas.txt ]; then
  echo "Clear up results_veridical/all_formulas.txt"
  rm results_veridical/all_formulas.txt
fi

echo "Translate sentences to formulas"
echo "Running semantics.sh"
./semantics.sh veridical >> results_veridical/all_formulas.txt

if [ -f results_veridical/all_sentences.txt ]; then
  echo "Clear up results_veridical/all_sentences.txt"
  rm results_veridical/all_sentences.txt
fi

for f in results_veridical/depth*.txt; do
  cat $f >> results_veridical/all_sentences.txt
done

paste results_veridical/all_sentences.txt results_veridical/all_formulas.txt > results_veridical/all_pairs.tsv

wc -l base_veridical/depth*

wc -l results_veridical/*

echo "Create tptp files and run prover"
./prove_veridical.sh results_veridical/all_pairs.tsv
# > $results_veridical/all_dataset
# > $results_veridical/all_dataset_rev

python embed.py results_veridical/all_dataset
# > $results_veridical/data_t_h.tsv
# > $results_veridical/data_ft_t.tsv
# > $results_veridical/data_ft_h.tsv

###

python format_prop.py --input=results_veridical/all_dataset
python format_veridicality.py --input=results_veridical

# Show the number of sentences
echo "#########################"
echo "Total number of sentences"
cat results_veridical/all_dataset | awk -F'\t' '{print $4}' | uniq -c | awk -F' ' '{print "depth"$2": "$1}'

echo "#########################"
echo "Total number of yes/unk labels"
cat results_veridical/all_dataset | awk -F'\t' '{print $4" "$6}' | sort | uniq -c | awk -F' ' '{print "depth"$2": "$3" "$1}'

echo "#########################"
echo "Total number of veridical inferences"
wc -l results_veridical/data_*.tsv
echo "Total number of (f(t), t) pairs"
cat results_veridical/data_ft_t.tsv | awk -F'\t' '{print $5}' | sort | uniq -c
echo "Total number of (t, h) pairs"
cat results_veridical/data_t_h.tsv | awk -F'\t' '{print $5}' | sort | uniq -c
echo "Total number of (f(t), h) pairs"
cat results_veridical/data_ft_h.tsv | awk -F'\t' '{print $5}' | sort | uniq -c

