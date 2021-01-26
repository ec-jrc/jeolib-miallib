Welcomne to miallib!

# Installation for pyjeo

This part explains how to install miallib as a dependency for pyjeo

## Build time dependencies

```
sudo apt update
DEBIAN_FRONTEND='noninteractive' sudo apt install -yq \
    git \
    build-essential \
    libgsl-dev \
    libgsl0-dev \
    libfann-dev \
    libgeotiff-dev \
    libfftw3-dev \
    libproj-dev \
    libjsoncpp-dev \
    libgdal-dev \
    libssl-dev \
    doxygen \
    swig \
    python \
    python-numpy \
    uthash-dev \
    libshp-dev
```

To build just the generic library for pyjeo:

```
make generic
```

## Install

```
sudo make install-generic
```

# Full installation (not for pyjeo)

This part explains how to install miallib (not only as a dependency for pyjeo)

## Build time dependencies

```
sudo apt update
DEBIAN_FRONTEND='noninteractive' sudo apt install -yq \
    git \
    build-essential \
    libgsl-dev \
    libgsl0-dev \
    libfann-dev \
    libgeotiff-dev \
    libfftw3-dev \
    libproj-dev \
    libjsoncpp-dev \
    libgdal-dev \
    libssl-dev \
    doxygen \
    swig \
    python \
    python-numpy \
    uthash-dev \
    libshp-dev \
    texlive
```

To build:

```
make build
```

## Install

```
sudo make install
```

## Usage

Upon successful completion of these steps, you can use miallib:

### Python
- from a python interpreter, simply type

``` python
import miallib
```

The doc can be found here: `/usr/local/share/doc/miallib/python/pdf/api.pdf`


### Lisp

Together with a LISP interpreter by typing:

```
mialisp /usr/local/share/mialisp/lsp/init.lsp
```

The doc can be found here: `/usr/local/share/doc/miallib/lisp/pdf/miadocxlisp.pdf`

### Others

You can also try to bind the miallib libraries directly with your preferred software:
```
/usr/local/lib/libmiallib_generic.so
```

The doc can be found here: `/usr/local/share/doc/miallib/lib/pdf/miallib.pdf`

