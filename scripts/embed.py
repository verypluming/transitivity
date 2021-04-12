
import csv
import pandas as pd
import argparse
import random

# file = 'results_veridical/all_dataset'

# 0 id
# 1 text: fred saw john and tom
# 2 hypothesis: fred saw john
# 3 depth: 1
# 4 occur: 1
# 5 label: yes

res_dir = 'results_veridical'

positive_clause_preds = ['realized', 'acknowledged', 'remembered', 'noted', 'found', 'noticed', 'learned', 'saw', 'revealed', 'discovered', 'understood', 'knew', 'admitted', 'recognized', 'observed']
neutral_clause_preds = ['felt', 'claimed', 'doubted', 'hoped', 'predicted', 'implied', 'suspected', 'wished', 'thought', 'believed', 'heard', 'expected', 'estimated', 'assumed', 'argued']
#positive_clause_preds = ['realized', 'knew', 'remembered']
#neutral_clause_preds = ['hoped', 'felt', 'mentioned']


def subj_schema(pred):
    return 'Someone ' + pred + ' that '


def filter_label(l1, l2):
    if l1 == 'yes' and l2 == 'yes':
        res = 'yes'
    else:
        res = 'unk'
    return res


if __name__ == '__main__':

    parser = argparse.ArgumentParser('generate veridical inference \
                                      sentence pairs')
    parser.add_argument('file',
                        help='input file')

    args = parser.parse_args()

    with open(args.file) as f:
        reader = list(csv.reader(f, delimiter='\t'))

        data_ft_t = []  # f(t) => t
        data_ft_h = []  # f(t) => h

        for row in reader:
            text = row[1].rstrip()
            hypo = row[2]
            depth = int(row[3])
            occur = int(row[4])
            label = row[5]
            for pred in positive_clause_preds:
                f_text = subj_schema(pred) + text
                p_label = 'yes'
                q_label = filter_label(p_label, label)
                data_ft_t.append([f_text, text, depth, occur, label, p_label])
                data_ft_h.append([f_text, hypo, depth, occur, label, q_label])
            for pred in neutral_clause_preds:
                f_text = subj_schema(pred) + text
                p_label = 'unk'
                q_label = 'unk'
                data_ft_t.append([f_text, text, depth, occur, label, p_label])
                data_ft_h.append([f_text, hypo, depth, occur, label, q_label])

        # Pruning (ft,t) pairs and (ft,h) pairs
        total = len(reader)
        data_ft_t_pruned = random.sample(data_ft_t, total)
        data_ft_h_pruned = random.sample(data_ft_h, total)

        with open(res_dir + '/data_ft_t.tsv', 'w') as fo:
            writer = csv.writer(fo, delimiter='\t')
            writer.writerows(data_ft_t_pruned)

        with open(res_dir + '/data_ft_h.tsv', 'w') as fo:
            writer = csv.writer(fo, delimiter='\t')
            writer.writerows(data_ft_h_pruned)

    df = pd.read_csv(args.file, delimiter='\t', header=None)
    data = df.drop(df.columns[0], axis=1)
    data.to_csv(res_dir + '/data_t_h.tsv', sep='\t', index=False, header=None)


# f(t) => t => g(a,b) => h(a,b)
#
# df = pd.read_csv('results_veridical/all_dataset', delimiter='\t', header=None)
# df[2] = df[2] + ',' + df[5]
# df[2] = df[2].str.split(',')
# df = df.groupby(1).agg({2: list, 3: list, 4: list})
# df = df.agg({2: lambda x: x,
#              3: lambda x: x[0],
#              4: lambda x: x[0]})
