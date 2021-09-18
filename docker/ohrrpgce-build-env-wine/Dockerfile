# bobthehamster/ohrrpgce-build-env-wine
# Run this with:
## docker run -v /full/path/to/src:/src -it bobthehamster/ohrrpgce-build-env-wine

# https://hub.docker.com/r/tobix/pywine
FROM tobix/pywine

RUN wine python -m pip install scons --no-warn-script-location

RUN apt-get install p7zip

WORKDIR /installers/

RUN curl -s -O http://mirror.motherhamster.org/dependencies/FreeBASIC-1.08.1-win64.zip \
  && unzip -q FreeBASIC-1.08.1-win64.zip \
  && mv FreeBASIC-1.08.1-win64 /opt/wineprefix/drive_c/FreeBASIC \
  && rm FreeBASIC-1.08.1-win64.zip

RUN curl -s -O http://mirror.motherhamster.org/dependencies/x86_64-8.1.0-release-win32-seh-rt_v6-rev0.7z \
  && p7zip -d x86_64-8.1.0-release-win32-seh-rt_v6-rev0.7z \
  && mv mingw64 /opt/wineprefix/drive_c/

# Install Euphoria into the path it was compiled to expect. Also duplicate eub to work around packaging weirdness
RUN curl -s -O http://mirror.motherhamster.org/dependencies/euphoria-4.1.0-Windows-x64-57179171dbed.zip \
  && unzip -q euphoria-4.1.0-Windows-x64-57179171dbed.zip \
  && mkdir -p /opt/wineprefix/drive_c/Euphoria-devel/ \
  && mv euphoria-4.1.0-Windows-x64 /opt/wineprefix/drive_c/Euphoria-devel/ \
  && cd /opt/wineprefix/drive_c/Euphoria-devel/euphoria-4.1.0-Windows-x64/ \
  && cp ./bin/eub.exe ./

WORKDIR /

RUN echo "export WINEPATH='C:/FreeBASIC;C:/mingw64/bin;C:/Euphoria-devel/euphoria-4.1.0-Windows-x64/bin'" >> /root/.bashrc

