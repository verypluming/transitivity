# transitivity
repository for our EACL2021 paper "Exploring Transitivity in Neural NLI Models through Veridicality"

## Naturalistic transitivity inference dataset

- `naturalistic/train.tsv` -- naturalistic inference datasets for training (MultiNLI format)

- `naturalistic/dev_matched.tsv` -- naturalistic inference datasets for testing (MultiNLI format)

It contains 
  - genre: yes/maybe indicates the label of veridical inference (basic inference one), and neutral/entailment indicates the label of original inference in SICK (basic inference two)
  - sentence1
  - sentence2
  - gold_label: automatically determined label

- `naturalistic/naturalistic_inference_human_performance.tsv` -- naturalistic inference datasets annotated with human performance.
It contains 
  - pairID: SICK ID
  - verb: clause-embedding predicate
  - sentence1
  - sentence2
  - gold_label: automatically determined label
  - avg_score: average score of three annotators' judgements
  - freq_label: discretized avg_score
  - crowd_score: three annotators' judgements
  - crowd_label: discretized three annotators' judgements

## Settings for generating full-synthetic transitivity inference dataset

Install vampire and [vampire](https://github.com/vprover/vampire) and [tregex](https://nlp.stanford.edu/software/tregex.shtml#Download) and set paths: Create files named `vampire_dir.txt` `tregex_location.txt`

```
sudo apt-get -y install swi-prolog
./install.sh
```

## Full-synthetic transitivity inference dataset

```
cd scripts
./run_veridical.sh pruning_rate(default: 100)
```

- `cfg_veridical.pl` -- a CFG to generate sentence schemas for a  propositional fragment
- `cfg_veridical_pn.pl` -- a CFG to generate concrete sentences for a propositional framgent (proper names instantiated)
- `generate_veridical.sh` -- script to run `cfg_veridical.pl`. Generated sentence schemas are in `base_veridical`.
- `instantiate_veridical.py` -- scrict to instantiate schema `np` in generated schemas. The first argument is the pruning rate. Generated sentencees are in `results_veridical`
- `semantics.sh` -- translate sentences to FOL formulas in `results_veridical` using `cfg_veridical_pn.pl`
- `prove_veridical.sh` -- extract an atomic formula H (and its negation) from each formula T and prove (T,H).
- `run_veridical.sh` -- run all these scripts and generate the MultiNLI format file. The result file is `results_veridical/train.tsv` and `results_veridical/train.tsv`.
It contains pairID, genre(the depth of the formula (depthX) and the number of connectives (booleanX), factive(f)/non-factive(nf), the label of original inference in the basic inference two(yes/unk)), sentence1, sentence2, gold_label.

## Citation
If you use this dataset and code in any published research, please cite the following:
* Hitomi Yanaka, Koji Mineshima, and Kentaro Inui. [Exploring Transitivity in Neural NLI Models through Veridicality](https://www.aclweb.org/anthology/2021.eacl-main.78.pdf) [arXiv](https://arxiv.org/pdf/2101.10713.pdf) Proceedings of the 16th conference of the European Chapter of the Association for Computational Linguistics (EACL2021), 2021.

```
@inproceedings{yanaka-etal-2021-exploring,
    title = "Exploring Transitivity in Neural {NLI} Models through Veridicality",
    author = "Yanaka, Hitomi  and
      Mineshima, Koji  and
      Inui, Kentaro",
    booktitle = "Proceedings of the 16th Conference of the European Chapter of the Association for Computational Linguistics: Main Volume",
    year = "2021",
    pages = "920--934",
}
```

## Contact
For questions and usage issues, please contact hyanaka@is.s.u-tokyo.ac.jp .

## License
Apache License


