# bobthehamster/freebasic
FROM debian:stretch-slim

ENV FBC_VERSION 1.05.0

ADD FreeBASIC-${FBC_VERSION}-linux-x86_64.tar.gz /freebasic-installer/

WORKDIR /freebasic-installer/FreeBASIC-${FBC_VERSION}-linux-x86_64/
RUN ./install.sh -i

WORKDIR /
RUN rm -rf /freebasic-installer
