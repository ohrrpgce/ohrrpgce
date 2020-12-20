# bobthehamster/ohrrpgce-build-env
# Begin with the freebasic image, (which is in turn based on debian:jessie-slim)

FROM bobthehamster/freebasic

# Install the packages needed to compile the ohrrpgce.
# Also include subversion for fetching the source.
# g++-multilib is needed for when we install euphoria later.
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
      g++-multilib \
      make \
      fakeroot \
    && rm -rf /var/lib/apt/lists/*

# Install the euphoria compiler. In spite of the deb name
# this is actually a 32 bit binary (which is why we need g++-multilib)

COPY euphoria_4.0.5_amd64.deb /euphoria-install/
RUN dpkg -i /euphoria-install/euphoria_4.0.5_amd64.deb
RUN rm -rf /euphoria-install
