mkdir -p compare
cat data/jeff | cut -d'|' -f1,3- > jeff_nocc; cat result/combined-stat| cut -d'|' -f1,3- > combine_nocc
mv *nocc compare 
diff compare/jeff_nocc compare/combine_nocc > difference_nocc
mv difference_nocc compare
