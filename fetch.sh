if [[ $# -lt 3 ]]; then 
    echo "Usage, fetch YYYY MM DD"
fi

YYYY=$1
MM=$(printf "%02d" $2)
DD=$(printf "%02d" $3)
BB=$(printf "%02d" $((DD-1)))

THE_DAY=$YYYY$MM$DD
DAY_BEFORE=$YYYY$MM$BB

mkdir -p data/$THE_DAY

cd data/$THE_DAY

curl http://ftp.apnic.net/stats/apnic/$YYYY/delegated-apnic-extended-$THE_DAY.gz --output apnic.gz
curl http://ftp.arin.net/pub/stats/arin/delegated-arin-extended-$DAY_BEFORE --output arin
curl http://ftp.afrinic.net/stats/afrinic/$YYYY/delegated-afrinic-extended-$THE_DAY --output afrinic
curl http://ftp.lacnic.net/pub/stats/lacnic/delegated-lacnic-extended-$DAY_BEFORE --output lacnic
curl https://ftp.ripe.net/pub/stats/ripencc/$YYYY/delegated-ripencc-extended-$DAY_BEFORE.bz2 --output ripencc.bz2 

bunzip2 ripencc.bz2
gunzip apnic.gz

