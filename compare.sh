if [ $# -eq 0 ] ; then 
    TODAY=$(date "+%Y%m%d")
else 
    TODAY=$1 
fi

mkdir -p compare/$TODAY
cat data/$TODAY/geoff | cut -d'|' -f1,3- > compare/$TODAY/geoff_nocc
cat result/$TODAY/combined-stat| cut -d'|' -f1,3- > compare/$TODAY/combine_nocc
cat result/$TODAY/merged-stat| cut -d'|' -f1,3- > compare/$TODAY/merged_nocc
diff compare/$TODAY/geoff_nocc compare/$TODAY/combine_nocc > compare/$TODAY/difference_nocc
diff compare/$TODAY/geoff_nocc compare/$TODAY/merged_nocc > compare/$TODAY/difference_merged_nocc

