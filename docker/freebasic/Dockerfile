# bobthehamster/freebasic
FROM debian:stretch-slim

ENV FBC_VERSION 1.07.3

ADD http://mirror.motherhamster.org/dependencies/FreeBASIC-${FBC_VERSION}-linux-x86_64.tar.gz /freebasic-installer/
WORKDIR /freebasic-installer/
RUN tar -zxf FreeBASIC-${FBC_VERSION}-linux-x86_64.tar.gz

WORKDIR /freebasic-installer/FreeBASIC-${FBC_VERSION}-linux-x86_64/
RUN ./install.sh -i

WORKDIR /
RUN rm -rf /freebasic-installer
