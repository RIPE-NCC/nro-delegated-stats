FROM openjdk:26-ea-21-slim

RUN apt-get update && apt-get install -y libdigest-sha-perl \
    && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /stats/data
WORKDIR /stats

COPY ./nro-delegated-stats.jar .
