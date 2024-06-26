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
    libgsl-dev \
    libgsl0-dev \
    libfann-dev \
    libgeotiff-dev \
    libfftw3-dev \
    libproj-dev \
    libjsoncpp-dev \
    libgdal-dev \
    libssl-dev \
    swig \
    uthash-dev \
    libopenblas-dev \
    libshp-dev
```

To build and install the miallib library via cmake for pyjeo:

```
mkdir build
cd build
cmake ..
cmake --build .
sudo cmake --install .
```