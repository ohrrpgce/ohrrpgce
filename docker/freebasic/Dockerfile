# bobthehamster/freebasic
FROM debian:stretch-slim

ENV FBC_VERSION 1.07.3

# First install the 64 bit version
ADD http://mirror.motherhamster.org/dependencies/FreeBASIC-${FBC_VERSION}-linux-x86_64.tar.gz /freebasic-installer/
WORKDIR /freebasic-installer/
RUN tar -zxf FreeBASIC-${FBC_VERSION}-linux-x86_64.tar.gz
WORKDIR /freebasic-installer/FreeBASIC-${FBC_VERSION}-linux-x86_64/
RUN ./install.sh -i

# Also add in the 32 bit libraries for multilib support
ADD http://mirror.motherhamster.org/dependencies/FreeBASIC-${FBC_VERSION}-linux-x86.tar.gz /freebasic-installer/
WORKDIR /freebasic-installer/
RUN tar -zxf FreeBASIC-${FBC_VERSION}-linux-x86.tar.gz
WORKDIR /freebasic-installer/FreeBASIC-${FBC_VERSION}-linux-x86/
RUN cp -r ./lib/freebasic/linux-x86 /usr/local/lib/freebasic/

WORKDIR /
RUN rm -rf /freebasic-installer
