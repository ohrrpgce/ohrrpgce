# bobthehamster/ohrrpgce-build-env-linux-x86_64

# Main image
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

# Compile and install Euphoria 4.2.0-pre
WORKDIR /euphoria-install/
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
      curl \
      git \
      ca-certificates \
      g++ \
      make \
    && curl -sS -O http://mirror.motherhamster.org/dependencies/euphoria-4.1.0-Linux-x64-57179171dbed.tar.gz \
    && tar -xzf euphoria-4.1.0-Linux-x64-57179171dbed.tar.gz -C /usr/local \
    && cd /usr/local/bin \
    && find /usr/local/euphoria-4.1.0-Linux-x64/bin -type f -executable -exec ln -s {} ';' \
    && eui --version
# Warning, the "make" command in this step is very slow!
# We checkout a specific commit to make sure the build is repeatable
RUN git clone https://github.com/OpenEuphoria/euphoria \
    && cd euphoria/source \
    && git checkout 5f3a0628287bea23b2e16ca3d388c37bc57df2f8 \
    && ./configure \
    && make
RUN cd euphoria/source \
    && build/eui --version \
    && make install
WORKDIR /
RUN rm -rf /euphoria-install

# Install the 64 bit version of Freebasic
WORKDIR /freebasic-installer/
RUN curl -sS http://mirror.motherhamster.org/dependencies/FreeBASIC-${FBC_VERSION}-linux-x86_64.tar.gz \
  | tar -zx
WORKDIR /freebasic-installer/FreeBASIC-${FBC_VERSION}-linux-x86_64/
RUN ./install.sh -i

WORKDIR /
RUN rm -rf /freebasic-installer

# Creating this and allowing non-root users to use it allows us to do basic command-line testing of ohrrpgce-game and ohrrpgce-custom
# like --help or --version
RUN mkdir /.ohrrpgce \
  ; chmod 777 /.ohrrpgce
