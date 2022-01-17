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

There are two main operations that can be performed. Generating the delegated extended stats, and notification/email
to existing Registry contacts of RIRs in case of persisting conflicts.

Here are the description of available command lines options.

```
NRO Extended Allocation and Assignments Statistics
Usage: java -jar nro-delegated-stats.jar [generate|notify]

Command: generate [options]
Generate NRO Delegated Extended Statistic, based on each RIRs delegated stats and IANA file
  -s, --startDate <value>  Start date for processing NRO delegated stat, default to today: YYYY-MM-DD
  -e, --endDate <value>    End date for processing NRO delegated stat, default to today: YYYY-MM-DD
  --ownIana                Use own generated IANA file as input, defaults to using http://ftp.apnic.net/stats/iana/delegated-iana-latest
Command: notify [options]
Notify RS contacts if there are persistent conflicts over a grace period
  -b, --base-url <value>   Base url for retrieving conflicts, defaults to: https://ftp.ripe.net/pub/stats/ripencc/nro-stats/.
  -c, --conflict-date <value>
                           Current conflict date, defaults to today: YYYY-MM-DD
```

For `generate` operation, by default the script will generate stats for today. 
You can configure to run it for past days by setting appropriate start/end date parameters.

For `notify` operation you need to provide `base-url` to fetch previous conflicts, with defaults to ftp.ripe.net.

Other application configuration that are not supplied via command line are provided in this [application.conf](https://github.com/RIPE-NCC/nro-delegated-stats/blob/main/src/main/resources/application.conf)
which by default is bundled, but can be overriden using `-Dconfig.file=<application.conf>`.

## License

Copyright (c) 2019-2022 RIPE NCC All rights reserved.

This software and all its separate source code is licensed under the terms of
the BSD 3-Clause License. If a copy of the license was not distributed to you,
you can obtain one at https://github.com/RIPE-NCC/nro-delegated-stats/blob/main/LICENSE.txt.
