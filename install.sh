#!/bin/bash
#
# Download Vampire (Version 4.4) from https://github.com/vprover/vampire
#

vampire_url="https://github.com/vprover/vampire/archive/4.4.tar.gz"
vampire_basename=`basename $vampire_url`

if [ ! -d vampire ]; then
  curl -LO $vampire_url
  tar -zxvf $vampire_basename
fi


rm -f $vampire_basename

# Make release version
cd ${vampire_dir}
make vampire_rel
cp vampire_rel_* vampire
echo `pwd`"/"vampire-4.4 > vampire_dir.txt

cd ..
tregex_url="https://nlp.stanford.edu/software/stanford-tregex-2018-10-16.zip"
tregex_basename=`basename $tregex_url`

if [ ! -d stanford-tregex-2018-10-16 ]; then
  curl -LO $tregex_url
  unzip $tregex_basename
fi

# Set path to stanford-tregex-2018-10-16 directory
tregex_dir=`pwd`"/"stanford-tregex-2018-10-16
echo $tregex_dir > tregex_location.txt

rm -f $tregex_basename
