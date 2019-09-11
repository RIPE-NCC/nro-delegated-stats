mkdir -p compare
cat data/geoff | cut -d'|' -f1,3- > compare/geoff_nocc
cat result/combined-stat| cut -d'|' -f1,3- > compare/combine_nocc
diff compare/geoff_nocc compare/combine_nocc > compare/difference_nocc

