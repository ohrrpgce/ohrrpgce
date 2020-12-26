# bobthehamster/ohrrpgce-mxe-build-env

FROM bobthehamster/ohrrpgce-build-env

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
      autoconf \
      automake \
      autopoint \
      bash \
      bison \
      bzip2 \
      flex \
      g++ \
      g++-multilib \
      gettext \
      git \
      gperf \
      intltool \
      libc6-dev-i386 \
      libgdk-pixbuf2.0-dev \
      libltdl-dev \
      libssl-dev \
      libtool-bin \
      libxml-parser-perl \
      lzip \
      make \
      openssl \
      p7zip-full \
      patch \
      perl \
      python \
      ruby \
      sed \
      unzip \
      wget \
      xz-utils \
      libffi-dev \
      libgmp3-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt
RUN git -c http.sslVerify=false clone https://github.com/mxe/mxe.git
WORKDIR /opt/mxe
RUN make gcc libffi
ENV PATH="${PATH}:/opt/mxe/usr/bin"

WORKDIR /opt
RUN git -c http.sslVerify=false clone https://github.com/rversteegen/fbc.git --branch android --single-branch
WORKDIR /opt/fbc

# This still doesn't work-- it is a work-in-progress :P
RUN make compiler install-compiler install-includes -j6 \
    && make rtlib install-rtlib gfxlib2 install-gfxlib2 MULTILIB=32 -j6 \
    && make rtlib install-rtlib gfxlib2 install-gfxlib2 MULTILIB=64 -j6 \
    && make unit-tests \
    && echo "Done make linux and unit tests..."

ENV TARGET="i686-w64-mingw32.static"
RUN rtlib install-rtlib gfxlib2 install-gfxlib2 TARGET=$TARGET prefix=$HOME/local/fbc-git CFLAGS="-g `$TARGET-pkg-config libffi --cflags`" \
    && echo "Done make windows target..."
    
