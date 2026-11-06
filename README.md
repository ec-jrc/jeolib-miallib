Welcome to miallib!

# Installation for pyjeo

This part explains how to install miallib as a dependency for pyjeo

## Build dependencies

```
sudo apt update
DEBIAN_FRONTEND='noninteractive' sudo apt install -yq \
    git \
    build-essential \
    cmake \
    libfftw3-dev \
    libgdal-dev \
    libgeotiff-dev \
    libgsl-dev \
    libproj-dev \
    libshp-dev \
    libtiff-dev \
    libtiff-dev \
    pkgconf \
    zlib1g-dev
```

To build and install the miallib library via cmake for pyjeo:

```
mkdir build
cd build
cmake ..
cmake --build .
sudo cmake --install .
```
