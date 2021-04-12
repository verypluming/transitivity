
import argparse
import glob

import collections
import math

def perm_count(n,r):
    return math.factorial(n) // math.factorial(n - r)

parser = argparse.ArgumentParser('count the number of sentences generated')
parser.add_argument('pn', type=int, help='numper of proper names')
parser.add_argument('rate', type=int, help='pruning rate')
args = parser.parse_args()

counts = collections.Counter()
for file in glob.glob("./base_prop/*.scheme"):
    with open(file) as f:
        finput = f.read()
        lines = finput.splitlines()
        for line in lines:
            count = line.count("np")
            counts.update([count])

counts_sorted = sorted(counts.most_common())

total = 0
for count,sen in counts_sorted:
    gen = (perm_count(args.pn,count) * sen) // args.rate
    print('np:{0}, sentence scheme: {1}, generated sentences: {2}'.format(count, sen, gen))
    total += gen

print('total: {0}'.format(total))

