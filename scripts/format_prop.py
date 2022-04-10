#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.

import glob
import pandas as pd
import re
import copy
import os
import sys
import random
import argparse


def trans_label(label):
    if label == "yes":
        return "entailment"
    elif label == "unk":
        return "neutral"

def check_pn(sentence):
    prop_n = {"ann":"Ann", "bob":"Bob", "chris":"Chris", "daniel":"Daniel", "elliot":"Elliot", "fred":"Fred", "greg":"Greg", "henry":"Henry", "tom":"Tom", "john":"John"}
    for pn, npn in prop_n.items():
        if pn in sentence:
            tmp1 = re.compile(pn)
            tmp2 = re.compile(npn)
            sentence = re.sub(pn, npn, sentence)
    return sentence

parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter)
parser.add_argument("--input", nargs='?', type=str, help="input file")
ARGS = parser.parse_args()
sentences = []
files = glob.glob(ARGS.input+"*")
for file in files:
    with open(file, "r") as f:
        for line in f:
            pair_id, s1, s2, depth, connect, label = line.split("\t")
            genre = "depth"+str(depth)+".boolean"+str(connect)
            s1 = check_pn(s1)
            s2 = check_pn(s2)
            if re.search("punct", s1):
                s1 = re.sub("\spunct", ",", s1)
            if re.search("punct", s2):
                s1 = re.sub("\spunct", ",", s2)
            s1 = s1[0].upper() + s1[1:]
            s1 = s1.strip()+"."
            s2 = s2[0].upper() + s2[1:]
            s2 = s2.strip()+"."
            sentences.append([genre, s1, s2, trans_label(label.strip())])

df = pd.DataFrame(sentences, columns=['genre', 'sentence1', 'sentence2', 'gold_label'])
train =pd.DataFrame(index=[], columns=['index','promptID','pairID','genre','sentence1_binary_parse','sentence2_binary_parse','sentence1_parse','sentence2_parse','sentence1','sentence2','label1','gold_label'])
train['index'] = df.index
train['promptID'] = df.index
train['pairID'] = df.index
train['gold_label'] = df["gold_label"]
train['genre'] = df["genre"]
train['sentence1'] = df["sentence1"]
train['sentence2'] = df["sentence2"]
final_train = train.sample(frac=1)
final_train.to_csv(ARGS.input+".tsv", sep="\t", index=False)