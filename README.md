NRO Delegated Stats
===================

Reverse engineering delegated stats normally produced by Geoff Houston from APNIC.

Goal is to reproduce: https://www.nro.net/wp-content/uploads/apnic-uploads/delegated-extended [1]

Input files are data from RIRs, and IANA files which is also from Jeff.

```
    APNIC   -> "https://ftp.apnic.net/stats/apnic/delegated-apnic-extended-latest",
    AFRINIC -> "https://ftp.afrinic.net/stats/afrinic/delegated-afrinic-extended-latest",
    ARIN    -> "https://ftp.arin.net/pub/stats/arin/delegated-arin-extended-latest",
    LACNIC  -> "https://ftp.lacnic.net/pub/stats/lacnic/delegated-lacnic-extended-latest",
    RIPENCC -> "https://ftp.ripe.net/pub/stats/ripencc/delegated-ripencc-extended-latest",
    IANA    -> "https://ftp.apnic.net/pub/stats/iana/delegated-iana-latest",
```

##

Merged results will be created in `result/combined-stat`
If there are conflicting entries between RIRs it will be stored in `result/conflicts`

We are not performing EU country code mapping that was done in the original NRO stats.
 
Building single jar assembly can be done with `sbt assembly` and look for `nro-delegated-stats.jar` in target directory.

Once the jar is build, there is optional JVM parameter that you can give, startDate and endDate e.g:

```
java -DstartDate=2019-11-01 -DendDate=2019-11-10 -jar nro-delegated-stats.jar

```

Not giving these parameters will run the program only for today. Giving only start day will run it up to today.

##


