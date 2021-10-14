FROM mozilla/sbt
RUN mkdir stats
WORKDIR ./stats
COPY ./nro-delegated-stats.jar .