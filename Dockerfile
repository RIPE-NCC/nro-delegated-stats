FROM eclipse-temurin:21-ubi10-minimal

RUN microdnf install -y libdigest-sha-perl

RUN mkdir -p /stats/data
WORKDIR /stats

COPY ./nro-delegated-stats.jar .
