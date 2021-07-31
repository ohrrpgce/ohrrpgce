# bobthehamster/freebasic-x86
FROM i386/debian:stretch-slim
# Using an i386 image on x86_64 produces a harmless warning, but works correctly (the image runs as 32 bit)

ENV FBC_VERSION 1.07.3

ADD http://mirror.motherhamster.org/dependencies/FreeBASIC-${FBC_VERSION}-linux-x86.tar.gz /freebasic-installer/
WORKDIR /freebasic-installer/
RUN tar -zxf FreeBASIC-${FBC_VERSION}-linux-x86.tar.gz

WORKDIR /freebasic-installer/FreeBASIC-${FBC_VERSION}-linux-x86/
RUN ./install.sh -i

WORKDIR /
RUN rm -rf /freebasic-installer
