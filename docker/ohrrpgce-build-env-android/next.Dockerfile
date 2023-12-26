#bobthehamster/ohrrpgce-build-env-android

# NOTE this doesn't work yet! -- 2023-12 James Paige

#-----------------------------------------------------------------------
# Android SDK with OHRRPGCE build-deps
# and both Freebasic and Euphoria Compilers

FROM thyrlian/android-sdk
# The base image is descended from Ubuntu 22.04 LTS (jammy)

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
      libx11-dev \
      libxpm-dev \
      libxrandr-dev \
      libxrender-dev \
      libsdl2-dev \
      libsdl2-mixer-dev \
      libsdl1.2-dev \
      libsdl-mixer1.2-dev \
      libncurses5-dev \
      scons \
      g++ \
      g++-multilib \
      make \
      fakeroot \
      libtinfo5 \
    && rm -rf /var/lib/apt/lists/*

#
# Freebasic
#

ENV FBC_VERSION 1.08.1

# First install the 64 bit version
WORKDIR /freebasic-installer/
RUN curl -sS http://mirror.motherhamster.org/dependencies/FreeBASIC-${FBC_VERSION}-linux-x86_64.tar.gz \
  | tar -zx
WORKDIR /freebasic-installer/FreeBASIC-${FBC_VERSION}-linux-x86_64/
RUN ./install.sh -i

# Also add in the 32 bit libraries for multilib support
WORKDIR /freebasic-installer/
RUN curl -sS http://mirror.motherhamster.org/dependencies/FreeBASIC-${FBC_VERSION}-linux-x86.tar.gz \
  | tar -zx
WORKDIR /freebasic-installer/FreeBASIC-${FBC_VERSION}-linux-x86/
RUN cp -r ./lib/freebasic/linux-x86 /usr/local/lib/freebasic/

WORKDIR /
RUN rm -rf /freebasic-installer

#
# Euphoria
#

# Install the euphoria compiler. In spite of the deb name
# this is actually a 32 bit binary (which is why we need g++-multilib)
WORKDIR /euphoria-install/
RUN curl -sS -O http://mirror.motherhamster.org/dependencies/euphoria_4.0.5_amd64.deb
RUN dpkg -i /euphoria-install/euphoria_4.0.5_amd64.deb
WORKDIR /
RUN rm -rf /euphoria-install

#
# Freebasic ARM support
#

WORKDIR /fbc-arm-install
RUN git clone --branch android https://github.com/rversteegen/fbc.git freebasic && \
  echo "CFLAGS := -Wfatal-errors -g" > /fbc-arm-install/freebasic/config.mk && \
  echo "FBCFLAGS := -g" >> /fbc-arm-install/freebasic/config.mk && \
  echo "FBLFLAGS := -g" >> /fbc-arm-install/freebasic/config.mk && \
  echo "prefix=/opt/fbc-arm/fbc-1.06-android" >> /fbc-arm-install/freebasic/config.mk && \
  cd /fbc-arm-install/freebasic && \
  make compiler && \
  make install-compiler install-includes

WORKDIR /
RUN rm -rf /fbc-arm-install

ENV FBCARM /opt/fbc-arm/fbc-1.06-android/bin/fbc

#
# Final stuff
#

# This is mounted as a volume by the wrapper script
ENV SDLANDROID /src/sdl-android

WORKDIR /src
# See also the wrapper script which sets up the correct volume mount and UID:GID
