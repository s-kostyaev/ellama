FROM debian:bookworm-slim

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    emacs-nox \
    make \
    nodejs \
    npm \
    bubblewrap \
    socat \
    ripgrep \
    git \
 && rm -rf /var/lib/apt/lists/*

ARG SRT_NPM_SPEC=@anthropic-ai/sandbox-runtime@latest
RUN npm install -g "${SRT_NPM_SPEC}"

WORKDIR /work

