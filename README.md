Welcomne to miallib!

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
    texlive
```


```
make build
```

The documentation generation for pymia used epydoc for python2.7 and sphinx for 3.6.

+ Caveat: for the documentation, if texlive version is >= 2019, doxygen >= 1.8.16 needs to be installed!

To build just the generic library without Python and Lisp for pyjeo:

```
make generic
```

### Local builds only!!!

Our docker containers use a customized gdal build which links to `libshp`.
When we use that, we shouldn't link `miallib` to `libshp`.

For local builds though, we also need to link `miallib` with `libshp`. So:

```
sudo apt install -y libshp-dev
```

## Install

```
sudo make install
```

To install just the generic library without Python and Lisp for pyjeo:

```
sudo make install-generic
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

