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

def check_factive(pred):
    positive_clause_preds = ['realized', 'acknowledged', 'remembered', 'noted', 'found', 'noticed', 'learned', 'saw', 'revealed', 'discovered', 'understood', 'knew', 'admitted', 'recognized', 'observed']
    neutral_clause_preds = ['felt', 'claimed', 'doubted', 'hoped', 'predicted', 'implied', 'suspected', 'wished', 'thought', 'believed', 'heard', 'expected', 'estimated', 'assumed', 'argued']
    #positive_clause_preds = ['realized', 'knew', 'remembered']
    #neutral_clause_preds = ['hoped', 'felt', 'mentioned']

    if pred in positive_clause_preds:
        return "f"
    elif pred in neutral_clause_preds:
        return "nf"

parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter)
parser.add_argument("--input", nargs='?', type=str, help="input file")
ARGS = parser.parse_args()
sentences = []
files = glob.glob(ARGS.input+"/data*.tsv")
for file in files:
    with open(file, "r") as f:
        for line in f:
            if re.search("data_t_h", file):
                s1, s2, depth, connect, label = line.split("\t")
                genre = "ph.depth"+str(depth)+".boolean"+str(connect)+"."+label.strip()
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
            elif re.search("data_ft_t", file):
                s1, s2, depth, connect, th_label, label = line.split("\t")
                genre = "fpp.depth"+str(depth)+".boolean"+str(connect)+"."+label.strip()
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
final_train.to_csv(ARGS.input+"/train.tsv", sep="\t", index=False)

sentences = []
with open(ARGS.input+"/data_ft_h.tsv", "r") as f:
    for line in f:
        s1, s2, depth, connect, th_label, label = line.split("\t")
        pred_label = check_factive(s1.split(" ")[1])
        genre = "depth"+str(depth)+".boolean"+str(connect)+"."+pred_label+"."+th_label
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

df2 = pd.DataFrame(sentences, columns=['genre', 'sentence1', 'sentence2', 'gold_label'])
test = pd.DataFrame(index=[], columns=['index','promptID','pairID','genre','sentence1_binary_parse','sentence2_binary_parse','sentence1_parse','sentence2_parse','sentence1','sentence2','label1','gold_label'])
test['index'] = df2.index
test['promptID'] = df2.index
test['pairID'] = df2.index
test['gold_label'] = df2["gold_label"]
test['genre'] = df2["genre"]
test['sentence1'] = df2["sentence1"]
test['sentence2'] = df2["sentence2"]
final_test = test.sample(frac=1)
final_test.to_csv(ARGS.input+"/dev_matched.tsv", sep="\t", index=False)