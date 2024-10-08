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

## Reports

More detailed description about the generated reports is documented [here](Reports.md).

## Usage

Building single jar assembly can be done with `sbt assembly` and look for `nro-delegated-stats.jar` in target directory.

There are two main operations that can be performed. Generating the delegated extended stats, and notification/email
to existing Registry contacts of RIRs in case of persisting conflicts.

Here are the description of available command lines options.

```
NRO Extended Allocation and Assignments Statistics
Usage: java -jar nro-delegated-stats.jar [generate|notify|iana-file] [options]

  -b, --base-url <value>   Base URL for retrieving previously generated files. Defaults to https://ftp.ripe.net/pub/stats/ripencc/nro-stats.
  -h, --help               print this message
Command: generate [options]
Generate NRO Delegated Extended Statistic, based on each RIRs delegated stats and IANA file
  -s, --startDate <value>  Start date for processing NRO delegated stat, default to today: YYYY-MM-DD
  -e, --endDate <value>    End date for processing NRO delegated stat, default to today: YYYY-MM-DD
  --ownIana                Use own generated IANA file as input, defaults to using http://ftp.apnic.net/stats/iana/delegated-iana-latest
Command: notify [options]
Notify RS contacts if there are persistent conflicts over a grace period
  -c, --conflict-date <value>
                           Current conflict date, defaults to today: YYYY-MM-DD
Command: iana-file
Generate IANA file based on  numbers and resources from iana.org
```

For `generate` operation, by default the script will generate stats for today. 
You can configure to run it for past days by setting appropriate start/end date parameters.

For `notify` operation you need to provide `base-url` to fetch previous conflicts, with defaults to ftp.ripe.net.

Other application configuration that are not supplied via command line are provided in this [application.conf](https://github.com/RIPE-NCC/nro-delegated-stats/blob/main/src/main/resources/application.conf)
which by default is bundled, but can be overriden using `-Dconfig.file=<application.conf>`.


## Adding conflict exceptions 

There are conflicts in certain resource attribution and these conflicts are mentioned in the `conflicts` generated 
in the result directory for every date. Some of these conflicts are historically justified in some way or the other
and are pretty hard to get rid of administratively. If RS gives up on some of the conflicts, we have `allowedlist` 
file to add them to. After adding resources to `allowedlist` they are not mentioned in the notification email sent 
to RS on the daily basis. The easiest way of suppressing conflict notifications is to copy-paste the line (any of 
the two conflicting lines) from `conflicts` to the `allowedlist` and rebuild the docker image by creating a new release.

## License

Copyright (c) 2019-2022 RIPE NCC All rights reserved.

This software and all its separate source code is licensed under the terms of
the BSD 3-Clause License. If a copy of the license was not distributed to you,
you can obtain one at https://github.com/RIPE-NCC/nro-delegated-stats/blob/main/LICENSE.txt.
