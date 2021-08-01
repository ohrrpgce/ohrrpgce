# bobthehamster/ohrrpgce-build-env-x86
# Begin with the freebasic-x86 image, (which is in turn based on i386/debian:stretch-slim)

FROM bobthehamster/freebasic

# Install the packages needed to compile the ohrrpgce.
# Also include subversion for fetching the source.
# We don't bother with timidity or freepats because those
# are really run-time requirements, not build-time.

RUN dpkg --add-architecture i386

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
      git \
      subversion \
      bzip2 \
      zip unzip \
      libx11-dev:i386 \
      libxpm-dev:i386 \
      libxrandr-dev:i386 \
      libxrender-dev:i386 \
      libsdl2-dev:i386 \
      libsdl2-mixer-dev:i386 \
      libsdl1.2-dev:i386 \
      libsdl-mixer1.2-dev:i386 \
      libncurses5-dev:i386 \
      scons \
      g++ \
      g++-multilib \
      make \
      fakeroot \
    && rm -rf /var/lib/apt/lists/*

# Install the euphoria compiler.

WORKDIR /euphoria-install/
RUN curl -O http://mirror.motherhamster.org/dependencies/euphoria_4.0.5_i386.deb
RUN dpkg -i /euphoria-install/euphoria_4.0.5_i386.deb
WORKDIR /
RUN rm -rf /euphoria-install
