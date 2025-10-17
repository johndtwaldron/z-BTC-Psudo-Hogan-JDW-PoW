FROM debian:stable-slim
# hadolint ignore=DL3008
RUN apt-get update && apt-get install -y --no-install-recommends \
    gnucobol make gcc ca-certificates \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /work