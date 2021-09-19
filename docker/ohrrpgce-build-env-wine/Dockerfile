# bobthehamster/ohrrpgce-build-env-wine
# Run this with:
## docker run -v /full/path/to/src:/src -it bobthehamster/ohrrpgce-build-env-wine

# https://hub.docker.com/r/tobix/pywine
FROM tobix/pywine

RUN wine python -m pip install scons --no-warn-script-location

RUN apt-get update \
    && apt-get install -q -y --no-install-recommends \
      p7zip \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /installers/

ARG FBCVER=FreeBASIC-1.08.1-win32
RUN curl -s -O http://mirror.motherhamster.org/dependencies/$FBCVER.zip \
  && unzip -q $FBCVER.zip \
  && mv $FBCVER /opt/wineprefix/drive_c/FreeBASIC \
  && rm $FBCVER.zip

ARG MINGWVER=i686-8.1.0-release-win32-dwarf-rt_v6-rev0
# This file lists links to 7zipped mingw64 builds:
# https://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/installer/repository.txt/download
RUN curl -s -O http://mirror.motherhamster.org/dependencies/$MINGWVER.7z \
  && p7zip -d $MINGWVER.7z \
  && mv mingw32 /opt/wineprefix/drive_c/

# Install Euphoria into the path it was compiled to expect. Also duplicate eub to work around packaging weirdness

ARG EUVER=euphoria-4.1.0-Windows-x86
ARG EUVEREND=-57179171dbed
RUN curl -s -O http://mirror.motherhamster.org/dependencies/$EUVER$EUVEREND.zip \
  && unzip -q $EUVER$EUVEREND.zip \
  && rm $EUVER$EUVEREND.zip \
  && mkdir -p /opt/wineprefix/drive_c/Euphoria-devel/ \
  && mv $EUVER /opt/wineprefix/drive_c/Euphoria-devel/ \
  && cd /opt/wineprefix/drive_c/Euphoria-devel/$EUVER/ \
  && cp ./bin/eub.exe ./

ARG GITVER=PortableGit-2.33.0.2-32-bit
RUN curl -s -O http://mirror.motherhamster.org/dependencies/$GITVER.7z.exe \
  && xvfb-run wine $GITVER.7z.exe -o"C:\git" -y \
  && rm $GITVER.7z.exe \
  && echo 'Ignore the build-time error about "X connection broken" it is harmless'

RUN cp /opt/wineprefix/drive_c/Python39/python.exe /opt/wineprefix/drive_c/Python39/python3.exe

WORKDIR /

RUN echo "export WINEPATH='C:/Python39;C:/Python39/Scripts;C:/FreeBASIC;C:/mingw32/bin;C:/Euphoria-devel/$EUVER/bin;C:/git/bin'" >> /root/.bashrc

