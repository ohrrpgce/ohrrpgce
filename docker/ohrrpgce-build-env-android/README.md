# Dockerfiles for Android OHRRPGCE Build Environment

## oldstyle

**andr-oldstyle.sh and oldstyle.Dockerfile**

These are meant as a stopgap to make it possible to build the
current (2023) OHRRPGCE Android nightly build without a VM.
They rely on a very old Android SDK from circa 2012

## newstyle

**andr-next.sh and next.Dockerfile**

For future attempts at getting things working with the latest sdk
(probably won't work until we have rebased our sdl-androud changes
 on top of pelya's upstream)
