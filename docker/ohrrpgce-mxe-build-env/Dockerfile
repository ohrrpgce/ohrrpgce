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
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt
RUN git -c http.sslVerify=false clone https://github.com/mxe/mxe.git
WORKDIR /opt/mxe
RUN make gcc libffi
ENV PATH="${PATH}:/opt/mxe/usr/bin"

WORKDIR /opt
RUN git -c http.sslVerify=false clone https://github.com/freebasic/fbc.git
WORKDIR /opt/fbc

# FIXME: this step doesn't work yet, fails with:
# src/rtlib/thread_call.c:33:17: fatal error: ffi.h: No such file or directory
RUN make TARGET=i686-w64-mingw32.static prefix=/usr/local/ rtlib gfxlib2 install-rtlib install-gfxlib2
