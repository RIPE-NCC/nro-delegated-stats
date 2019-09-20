mkdir -p compare
cat data/geoff | cut -d'|' -f1,3- > compare/geoff_nocc
cat result/combined-stat| cut -d'|' -f1,3- > compare/combine_nocc
cat result/merged-stat| cut -d'|' -f1,3- > compare/merged_nocc
diff compare/geoff_nocc compare/combine_nocc > compare/difference_nocc
diff compare/geoff_nocc compare/merged_nocc > compare/difference_merged_nocc

