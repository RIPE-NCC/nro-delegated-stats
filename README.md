NRO Delegated Stats
===================

Reverse engineering delegated stats normally produced by Geoff Houston from APNIC.

Goal is to reproduce: https://www.nro.net/wp-content/uploads/apnic-uploads/delegated-extended [1]

Input files are data from RIRs, and IANA files which is also from Jeff.

```
    APNIC   -> "http://ftp.apnic.net/stats/apnic/delegated-apnic-extended-latest",
    AFRINIC -> "http://ftp.afrinic.net/stats/afrinic/delegated-afrinic-extended-latest",
    ARIN    -> "http://ftp.arin.net/pub/stats/arin/delegated-arin-extended-latest",
    LACNIC  -> "http://ftp.lacnic.net/pub/stats/lacnic/delegated-lacnic-extended-latest",
    RIPENCC -> "https://ftp.ripe.net/pub/stats/ripencc/delegated-ripencc-extended-latest",
    IANA    -> "http://ftp.apnic.net/pub/stats/iana/delegated-iana-latest",
```

##

Running this project can be done by using `run.sh`  script that will call sbt with enough memory.

Merged results will be created in `result/combined-stat`
If there are conflicting entries between RIRs it will be stored in `result/conflicts`

When there is a conflict of resources contained in more than one RIRs it will be printed out in console for now.


There is `compare.sh` script that will try to compare it with `data/geoff` which was the target file [1] we are trying
 to reverse engineer. Output of this script will be on `compare/difference_nocc`.
 
Comparison is done ignoring country code (second column), since Jeff has special AS number mapping for EU country code.
In this code we keep EU country code without trying to map it to the country.

Building single jar assembly can be done with `sbt assembly` and look for `nro-delegated-stats.jar` in target directory.
##


