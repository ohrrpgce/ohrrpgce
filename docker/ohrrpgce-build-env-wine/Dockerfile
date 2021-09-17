# bobthehamster/ohrrpgce-build-env-wine
# Run this with:
## docker run -v /full/path/to/src:/src -it bobthehamster/ohrrpgce-build-env-wine

# https://hub.docker.com/r/tobix/pywine
FROM tobix/pywine

RUN wine python -m pip install scons --no-warn-script-location

WORKDIR /installers/
RUN curl -s -O http://mirror.motherhamster.org/dependencies/FreeBASIC-1.08.1-win64.zip \
  && unzip -q FreeBASIC-1.08.1-win64.zip \
  && mv FreeBASIC-1.08.1-win64 /opt/wineprefix/drive_c/FreeBASIC \
  && rm FreeBASIC-1.08.1-win64.zip

WORKDIR /

RUN echo "export WINEPATH='C:\FreeBASIC'" >> /root/.bashrc

