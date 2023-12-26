#bobthehamster/ohrrpgce-build-env-android-oldstyle

#-----------------------------------------------------------------------
# Old-style Android SDK with OHRRPGCE build-deps
# and both Freebasic and Euphoria Compilers

FROM debian:11-slim

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
      ca-certificates \
      less \
      nano \
      ant \
      file \
      gpg \
      imagemagick \
    && rm -rf /var/lib/apt/lists/*

# Correto is Amazon's LTS distribution of OpenJDK 8.
# This seems to be the easiest way to get openjdk8 on Debian 10+
# because only openjdk11 is available on debian+ and our crusty old
# Android SDK is too old for openjdk11
RUN curl -sS https://apt.corretto.aws/corretto.key \
  | gpg --dearmor -o /usr/share/keyrings/corretto-keyring.gpg \
  && echo "deb [signed-by=/usr/share/keyrings/corretto-keyring.gpg] https://apt.corretto.aws stable main" \
  > /etc/apt/sources.list.d/corretto.list \
  && apt-get update \
  && apt-get install -y java-1.8.0-amazon-corretto-jdk \
  && rm -rf /var/lib/apt/lists/* \
  && update-java-alternatives -s /usr/lib/jvm/java-1.8.0-amazon-corretto

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

RUN mkdir -p /src/ && \
  cp -pr /fbc-arm-install/freebasic/src/rtlib /src/fbc-arm-rtlib

WORKDIR /
RUN rm -rf /fbc-arm-install

ENV FBCARM /opt/fbc-arm/fbc-1.06-android/bin/fbc

#
# Symlinks fixer
#

COPY fix_symlinks.sh /src/fix_symlinks.sh
RUN chmod +x /src/fix_symlinks.sh

#
# Final stuff
#

# This is mounted as a volume by wrapper script
ENV SDLANDROID /src/sdl-android
ENV ANDROIDNDK /opt/android-ndk-r12b
ENV ANDROIDNDKVER r12b
ENV ANDROIDAPI 26
ENV ANDROIDSDK /opt/android-sdk
ENV ANDROID_HOME /opt/android-sdk
ENV PATH="${PATH}:/opt/android-sdk/platform-tools:/opt/android-sdk/tools:/opt/android-ndk-r12b:/opt/android-sdk/build-tools/26.0.3"

WORKDIR /src
# See also wrapper script which sets up the correct volume mount and UID:GID
