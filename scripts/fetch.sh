#!/usr/bin/env bash

if [[ $# -lt 3 ]]; then 
    echo "Usage, fetch YYYY MM DD"
    exit 1
fi

cd /home/app-admin/nro-stats
YYYY=$1

#Padding zeros
MM=$(printf "%02d" $((10#$2)))
DD=$(printf "%02d" $((10#$3)))
#We're fetching archive, matching what we retrieved when
#We run this job at 13:00 CEST.
THE_DAY=$YYYY$MM$DD
DAY_BEFORE=$(date -d "$YYYY-$MM-$DD 1 day ago" +'%Y%m%d')

mkdir -p data/$THE_DAY

cd data/$THE_DAY

echo "Fetching http://ftp.apnic.net/stats/apnic/$YYYY/delegated-apnic-extended-$THE_DAY.gz"

#Timeout 10 seconds
CONNECT_TIMEOUT=10
#How much retry
RETRIES_COUNT=5
# Exponential backoff
RETRY_DELAY=0

curl --connect-timeout $CONNECT_TIMEOUT --retry $RETRIES_COUNT --retry-delay $RETRY_DELAY http://ftp.apnic.net/stats/apnic/$YYYY/delegated-apnic-extended-$THE_DAY.gz --output apnic.gz
curl --connect-timeout $CONNECT_TIMEOUT --retry $RETRIES_COUNT --retry-delay $RETRY_DELAY http://ftp.arin.net/pub/stats/arin/delegated-arin-extended-$DAY_BEFORE --output arin
curl --connect-timeout $CONNECT_TIMEOUT --retry $RETRIES_COUNT --retry-delay $RETRY_DELAY http://ftp.afrinic.net/stats/afrinic/$YYYY/delegated-afrinic-extended-$THE_DAY --output afrinic
curl --connect-timeout $CONNECT_TIMEOUT --retry $RETRIES_COUNT --retry-delay $RETRY_DELAY http://ftp.lacnic.net/pub/stats/lacnic/delegated-lacnic-extended-$DAY_BEFORE --output lacnic
curl --connect-timeout $CONNECT_TIMEOUT --retry $RETRIES_COUNT --retry-delay $RETRY_DELAY https://ftp.ripe.net/pub/stats/ripencc/$YYYY/delegated-ripencc-extended-$DAY_BEFORE.bz2 --output ripencc.bz2

# Special case for iana
wget -4 ftp://ftp.apnic.net/pub/stats/iana/delegated-iana-latest -O iana

# Will override existing bz2
bunzip2 -f ripencc.bz2
gunzip apnic.gz

