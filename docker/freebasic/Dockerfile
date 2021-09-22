# bobthehamster/freebasic
FROM debian:stretch-slim

ENV FBC_VERSION 1.08.1

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
      curl

# First install the 64 bit version
WORKDIR /freebasic-installer/
RUN curl http://mirror.motherhamster.org/dependencies/FreeBASIC-${FBC_VERSION}-linux-x86_64.tar.gz \
  | tar -zx
WORKDIR /freebasic-installer/FreeBASIC-${FBC_VERSION}-linux-x86_64/
RUN ./install.sh -i

# Also add in the 32 bit libraries for multilib support
WORKDIR /freebasic-installer/
RUN curl http://mirror.motherhamster.org/dependencies/FreeBASIC-${FBC_VERSION}-linux-x86.tar.gz \
  | tar -zx
WORKDIR /freebasic-installer/FreeBASIC-${FBC_VERSION}-linux-x86/
RUN cp -r ./lib/freebasic/linux-x86 /usr/local/lib/freebasic/

WORKDIR /
RUN rm -rf /freebasic-installer
