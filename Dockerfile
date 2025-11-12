FROM eclipse-temurin:21-ubi10-minimal

RUN apt-get update && apt-get install -y libdigest-sha-perl \
    && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /stats/data
WORKDIR /stats

COPY ./nro-delegated-stats.jar .
