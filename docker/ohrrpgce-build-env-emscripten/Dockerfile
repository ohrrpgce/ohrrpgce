#bobthehamster/ohrrpgce-build-env-emscripten

#-----------------------------------------------------------------------
# Emscripten with OHRRPGCE build-deps
# and both Freebasic and Euphoria Compilers

# Seems to descend from Ubuntu Jammy
FROM emscripten/emsdk:latest

#
# Apt packages
#

# Install the packages needed to compile the ohrrpgce.
# Also include git and subversion because build scripts use them.
# g++-multilib is needed for when we install euphoria later.
# We don't bother with timidity or freepats because those
# are really run-time requirements, not build-time.

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
      curl \
      git \
      subversion \
      bzip2 \
      zip unzip \
      libncurses5-dev \
      scons \
      make \
      libtinfo5 \
      less \
      nano \
    && rm -rf /var/lib/apt/lists/*

#      libx11-dev \
#      libxpm-dev \
#      libxrandr-dev \
#      libxrender-dev \
#      libsdl2-dev \
#      libsdl2-mixer-dev \
#      libsdl1.2-dev \
#      libsdl-mixer1.2-dev \
##      libncurses5-dev \
##      scons \
#      g++ \
#      g++-multilib \
##      make \
#      fakeroot \
##      libtinfo5 \


#
# Freebasic
#

ENV FBC_VERSION 1.08.1

# First install the 64 bit version
WORKDIR /freebasic-installer/
RUN curl -sS http://mirror.motherhamster.org/dependencies/FreeBASIC-1.08.1-linux-x86_64.tar.gz \
  | tar -zx
WORKDIR /freebasic-installer/FreeBASIC-${FBC_VERSION}-linux-x86_64/
RUN ./install.sh -i

# Also add in the 32 bit libraries for multilib support
WORKDIR /freebasic-installer/
RUN curl -sS http://mirror.motherhamster.org/dependencies/FreeBASIC-1.08.1-linux-x86.tar.gz \
  | tar -zx
WORKDIR /freebasic-installer/FreeBASIC-${FBC_VERSION}-linux-x86/
RUN cp -r ./lib/freebasic/linux-x86 /usr/local/lib/freebasic/

WORKDIR /
RUN rm -rf /freebasic-installer

#
# Freebasic with emscripten support
#

WORKDIR /fbc-tmc-source
RUN git clone --branch emscripten https://github.com/rversteegen/fbc.git freebasic && \
  echo "CFLAGS := -Wfatal-errors -g" > /fbc-tmc-source/freebasic/config.mk && \
  echo "FBCFLAGS := -g" >> /fbc-tmc-source/freebasic/config.mk && \
  echo "FBLFLAGS := -g" >> /fbc-tmc-source/freebasic/config.mk && \
  echo "prefix=/opt/fbc-emscripten" >> /fbc-tmc-source/freebasic/config.mk && \
  cd /fbc-tmc-source/freebasic && \
  make -j4 install-compiler install-includes && \
  make -j4 install-rtlib TARGET=asmjs-unknown-emscripten

WORKDIR /
RUN rm -rf /fbc-tmc-source

# Put the emscripten fbc in the front of the path so it will shadow the other fbc
ENV PATH /opt/fbc-emscripten/bin:${PATH}

#
# Final stuff
#

WORKDIR /src
# See also wrapper script which sets up the correct volume mount and UID:GID
