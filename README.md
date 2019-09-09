Initial README.

It is supposed to be a project for merging NRO stats from different sources.


Before converting to Streaming version which should consume less memory, you might need this option in SBT:
```
export SBT_OPTS="-Xmx2G -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=2G -Xss2M  -Duser.timezone=GMT"
```

Main is in Stats.
