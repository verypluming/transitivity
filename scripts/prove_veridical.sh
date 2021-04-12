#!/bin/bash

# ./prove_veridical.sh results_veridical/all_pairs_tsv

file=$1

if [ ! -f vampire_dir.txt ]; then
  "vampire_dir.txt does not exist."
  exit 1
fi

vampire_dir=$(cat vampire_dir.txt)
results_dir="results_veridical"

# cp -r results_prop $results_dir

# semantics="cfg_prop_pn.pl"

tptp="tptp_veridical"

lines=$(cat ${file} | wc -l)
total=$(echo $((${lines} * 2)))

if [ -d $tptp ]; then
   echo "Clear up tptp directory"
   rm -rf $tptp
fi

if [ -f ${results_dir}/pairs.txt ]; then
   echo "Clear up results file"
   rm ${results_dir}/pairs.txt
fi

if [ -f ${results_dir}/ans.txt ]; then
   echo "Clear up results file"
   rm ${results_dir}/ans.txt
fi

mkdir -p $tptp

function call_vampire(){
  tptp=$1
  ${vampire_dir}/vampire $tptp \
    | head -n 1 \
    | awk '{if($0 ~ "Refutation found"){ans="yes"} else {ans="unk"} print ans}'
  }

IFS=$'\n'

cat $file | \
while read line; do
  let id++
  sentence=$(echo $line | awk -F'\t' '{print $1}')
  formula=$(echo $line | awk -F'\t' '{print $2}')
  depth=$(echo $line | awk -F'\t' '{print $3}')
  occur=$(echo $line | awk -F'\t' '{print $4}')
  subformulas=$(echo $line | awk -F'\t' '{print $5}')

  text=$(swipl -s fol2tptp.pl -g "fol2tptp(${formula},user)" -t halt --quiet)

  for sf in $(swipl -s countDepth.pl -g "printElement(${subformulas})" -t halt --quiet); do
    let count++
    echo "Processing ${id}-${count}/${total}"
    hypo=$(swipl -s fol2tptp.pl -g "fol2tptp(${sf},user)" -t halt --quiet)
    echo -e "fof(t,axiom,${text})." \
      > ${tptp}/${id}-${count}.tptp
    echo -e "fof(h,conjecture,${hypo})." \
      >> ${tptp}/${id}-${count}.tptp

    echo -e "fof(t,axiom,${hypo})." \
      > ${tptp}/${id}-${count}.rev
    echo -e "fof(h,conjecture,${text})." \
      >> ${tptp}/${id}-${count}.rev

    sen=$(swipl -s cfg_veridical_pn.pl -g "verbalize(${sf})" -t halt --quiet)
    echo -e ${id}-${count}"\t"${sentence}"\t"${sen}"\t"${depth}"\t"${occur} >> ${results_dir}/pairs.txt
    echo -e ${id}-${count}"\t"${sen}"\t"${sentence}"\t"${depth}"\t"${occur} >> ${results_dir}/pairs.rev.txt
    # echo -e ${id}-${count}"\t"${sentence}"\t"${formula}"\t"${sen}"\t"${sf}"\t"${depth}"\t"${occur} >> ${results_dir}/pairs.txt
  done
done

for f in $(find ${tptp} -type f | grep ".tptp" | xargs ls |sort -k 2 -t "/" -n); do
  basename=$(basename ${f})
  echo "Prove ${basename}"
  answer=$(call_vampire $f)
  echo -e ${answer} >> ${results_dir}/ans.txt
done

for f in $(find ${tptp} -type f | grep ".rev" | xargs ls |sort -k 2 -t "/" -n); do
  basename=$(basename ${f})
  echo "Prove ${basename}"
  answer=$(call_vampire $f)
  echo -e ${answer} >> ${results_dir}/ans.rev.txt
done

wc -l ${results_dir}/pairs.txt
wc -l ${results_dir}/ans.txt

wc -l ${results_dir}/pairs.rev.txt
wc -l ${results_dir}/ans.rev.txt

# echo "Number of unknown examples:"
# grep -c "unk" ${results_dir}/ans.txt
# echo "Number of proved examples:"
# grep -c "yes" ${results_dir}/ans.txt

paste ${results_dir}/pairs.txt ${results_dir}/ans.txt > ${results_dir}/all_dataset

paste ${results_dir}/pairs.rev.txt ${results_dir}/ans.rev.txt > ${results_dir}/all_dataset_rev

# cat results_prop/all_dataset | awk -F'\t' '{print $4" "$6}' | sort -n | uniq -c
