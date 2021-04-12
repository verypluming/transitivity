#!/usr/bin/python
# -*- coding: utf-8 -*-

import argparse
import sys
import glob
import os.path
import itertools
import math
import random

base_dir = "base_veridical"
res_dir = "results_veridical"

## Lexical items ##

proper_names = ["ann", "bob", "chris", "daniel", "elliot", "fred", "greg", "henry", "tom", "john"]

# "mary", "kathy", "robert", "smith", "nancy"]

# # intransitive verbs
# base_ivs = ["walk", "run", "swim", "sleep", "leave"]
# past_ivs = ["walked", "ran", "swam", "slept", "left"]

# # transitive verbs
# base_tvs = ["see", "visit", "meet", "know", "touch"]
# past_tvs = ["saw", "visited", "met", "knew", "touched"]


def schematize(sentence):
    count = 0
    sent = []
    for word in sentence.split(' '):
        if word == 'np':
            var = '{0[' + str(count) + ']}'
            word = word.replace('np', var)
            count += 1
        sent.append(word)
    out = ' '.join(sent)
    return out


def instantiate(cat, items, finput, fout, rate):
    lines = finput.splitlines()
    output = []
    for line in lines:
        num = line.count(cat)
        perms = list(itertools.permutations(items, num))
        par = math.ceil(len(perms) * rate)
        perms = random.sample(perms, k=par)
        for ins in perms:
            sentence = schematize(line).format(ins)
            output.append(sentence)
    # output = output[::rate]
    output = '\n'.join(output) + '\n'
    with open(fout, 'a') as f:
        f.write(output)


def instantiate_(cat, items, sentence):
    num = sentence.count(cat)
    for i in itertools.permutations(items, num):
        sentence = schematize(sentence).format(i)
    return sentence


def perm_count(n, m):
    return math.factorial(n) // math.factorial(n - m)


def estimate(file, list):
    ''' Take a file with (depth, number of np, count) as in:
    $ cat base_veridical/uniq
    0 1 20
    0 2 20
    1 1 40
    and a list of names;
    Return the estimated number of sentences with each depth
    '''
    with open(file) as f:
        finput = f.readlines()
        res = {}
        depth = 0
        count = 0
        pn = len(list)
        for item in finput:
            pair = item.split(' ')
            dep = int(pair[0])
            np = int(pair[1])
            num = int(pair[2].rstrip())
            num = num * perm_count(pn, np)
            if dep == depth:
                count = count + num
            else:
                res[depth] = count
                depth += 1
                count = num
        res[depth] = count
        return res


if __name__ == '__main__':
    if os.path.exists(res_dir):
        print("Results directory already exists!")
        sys.exit()

    if not os.path.exists(base_dir):
        print("base_veridical directory does not exists!\ntry: generate_veridical.sh")
        sys.exit()

    os.mkdir(res_dir)

    parser = argparse.ArgumentParser('instantiate sentence schemas \
                                      with a pruning rate')
    parser.add_argument('total',
                        help='the total number of sentences \
                              generated for each depth', type=int)
    args = parser.parse_args()
    depth_count = estimate("./base_veridical/uniq", proper_names)

    for file in glob.glob("./base_veridical/depth*.scheme"):
        basename = os.path.basename(file)
        resname = basename.replace('scheme', 'txt')
        fout = res_dir + '/' + resname

        depth = int(basename.lstrip('depth').rstrip('.scheme'))
        if depth_count[depth] < args.total:
            rate = 1
        else:
            rate = args.total / depth_count[depth]

        with open(file) as f:
            finput = f.read()
            instantiate('np', proper_names, finput, fout, rate)
