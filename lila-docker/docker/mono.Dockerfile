##################################################################################
FROM node:24-trixie AS node

COPY repos/lila /lila
COPY conf/mono.conf /lila/conf/mono.conf
ENV COREPACK_ENABLE_DOWNLOAD_PROMPT=0
RUN corepack enable \
    && /lila/ui/build --clean --debug \
    && test -s /lila/public/npm/stockfish-web-move-review/sf_18_smallnet_single.js \
    && test -s /lila/public/npm/stockfish-web-move-review/sf_18_smallnet_single.wasm \
    && test -s /lila/public/npm/stockfish-web/sf_18_smallnet.js \
    && test -s /lila/public/npm/stockfish-web/sf_18_smallnet.wasm \
    && echo '4ca89e4b3abfbe9df13e4f3db2acb64dc6ddc7a9becb2ac1cf388f4d66b3bd94  /lila/public/lifat/nnue/nn-4ca89e4b3abf.nnue' | sha256sum -c -

##################################################################################
FROM sbtscala/scala-sbt:eclipse-temurin-alpine-25_36_1.11.6_3.7.3 AS lilabuilder

COPY --from=node /lila /lila
WORKDIR /lila
RUN ./lila.sh stage

##################################################################################
FROM mongo:7-jammy

RUN apt update \
    && apt install -y debian-keyring debian-archive-keyring apt-transport-https curl \
    && curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/gpg.key' | gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg \
    && curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt' | tee /etc/apt/sources.list.d/caddy-stable.list \
    && apt update \
    && apt install -y \
        caddy \
        curl \
        redis \
        supervisor \
    && apt clean \
    && mkdir -p /seeded /var/log/supervisor

COPY --from=lilabuilder /lila/target /lila/target
COPY --from=lilabuilder /lila/public /lila/public
COPY --from=lilabuilder /lila/conf   /lila/conf
COPY --from=node /lila/public /lila/target/universal/stage/public

COPY conf/supervisord.conf /etc/supervisor/conf.d/supervisord.conf
COPY conf/mono.Caddyfile /mono.Caddyfile
COPY static /static

ENV JAVA_HOME=/opt/java/openjdk
ENV JAVA_OPTS="-Xms4g -Xmx4g"
ENV PATH="${JAVA_HOME}/bin:${PATH}"
ENV LANG=C.utf8
COPY --from=eclipse-temurin:25-jdk $JAVA_HOME $JAVA_HOME

ENV CHESSTORY_DOMAIN=localhost:8080
ENV CHESSTORY_URL=http://localhost:8080

CMD ["supervisord", "-c", "/etc/supervisor/supervisord.conf"]
