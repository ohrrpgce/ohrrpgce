# bobthehamster/ohrrpgce-build-env-x86
# Begin with the freebasic-x86 image, (which is in turn based on i386/debian:stretch-slim)

FROM bobthehamster/freebasic-x86

# Install the packages needed to compile the ohrrpgce.
# Also include subversion for fetching the source.
# We don't bother with timidity or freepats because those
# are really run-time requirements, not build-time.

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
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
      make \
      fakeroot \
    && rm -rf /var/lib/apt/lists/*

# Install the euphoria compiler.

ADD http://mirror.motherhamster.org/dependencies/euphoria_4.0.5_i386.deb /euphoria-install/
RUN dpkg -i /euphoria-install/euphoria_4.0.5_i386.deb
RUN rm -rf /euphoria-install
