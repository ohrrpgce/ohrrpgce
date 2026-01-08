# bobthehamster/ohrrpgce-build-env-linux-x86_64

FROM debian:11-slim

ENV FBC_VERSION 1.08.1
ENV OHR_SKIP_X86 Yes

# Install the packages needed to compile the ohrrpgce.
# We don't bother with timidity or freepats because those
# are really run-time requirements, not build-time.
# Git and subversion are included so the build system can get revision numbers

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
      curl \
      git \
      subversion \
      bzip2 \
      zip unzip \
      libx11-dev \
      libxpm-dev \
      libxrandr-dev \
      libxrender-dev \
      libsdl2-dev \
      libsdl2-mixer-dev \
      libsdl1.2-dev \
      libsdl-mixer1.2-dev \
      libncurses5-dev \
      libncurses5 \
      scons \
      g++ \
      g++-multilib \
      make \
      fakeroot \
      less \
      nano \
    && rm -rf /var/lib/apt/lists/*

# First install the 64 bit version
WORKDIR /freebasic-installer/
RUN curl -sS http://mirror.motherhamster.org/dependencies/FreeBASIC-${FBC_VERSION}-linux-x86_64.tar.gz \
  | tar -zx
WORKDIR /freebasic-installer/FreeBASIC-${FBC_VERSION}-linux-x86_64/
RUN ./install.sh -i

WORKDIR /
RUN rm -rf /freebasic-installer

# Install the euphoria compiler.

WORKDIR /euphoria-install/
RUN curl -sS -O http://mirror.motherhamster.org/dependencies/euphoria_4.0.5_amd64.deb
RUN dpkg -i /euphoria-install/euphoria_4.0.5_amd64.deb
WORKDIR /
RUN rm -rf /euphoria-install

# Creating this and allowing non-root users to use it allows us to do basic command-line testing of ohrrpgce-game and ohrrpgce-custom
# like --help or --version
RUN mkdir /.ohrrpgce \
  ; chmod 777 /.ohrrpgce
