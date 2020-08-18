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
    texlive
```


```
make build
```

### Local builds only!!!

Our docker containers use a customized gdal build which links to `libshp`.
When we use that, we shouldn't link `miallib` to `libshp`.

For local builds though, we also need to link `miallib` with `libshp`. So:

```
sudo apt install -y libshp-dev
```

and then apply this patch:

```
diff --git a/core/c/Makefile b/core/c/Makefile
index 113f117..bec3be1 100644
--- a/core/c/Makefile
+++ b/core/c/Makefile
@@ -103,7 +103,7 @@ INCLUDE    = -I./ -I/usr/include/python2.7/ -I/usr/include/gdal/

 # TODO: add -lcsa -lnn

-LIBS       = -lgdal -ltiff -ldl -lproj -lfftw3_omp -lgsl -lgslcblas
+LIBS       = -lgdal -ltiff -ldl -lproj -lfftw3_omp -lgsl -lgslcblas -lshp
 INTERFACE  = imem.i
 SWIGOPT    =
 SWIG       = swig
diff --git a/xlisp/c/Makefile b/xlisp/c/Makefile
index 8e6910a..e1bb369 100755
--- a/xlisp/c/Makefile
+++ b/xlisp/c/Makefile
@@ -25,7 +25,7 @@ CFLAGS = -DXLISP -DPOSIX -DUNIX -DEDITOR -DMIAL -DALLFUNCTIONS -DTEST2 -DMCISRG


 $(BUILDDIR)/mialisp: banner_miallib.h $(BUILDOBJS) $(LIBMIALDIR)/libmiallib_xlisp.a
-       g++ -fopenmp -m64 -o $(BUILDDIR)/mialisp $(BUILDOBJS) $(LIBMIALDIR)/libmiallib_xlisp.a -lz -ltiff -lgeotiff -lgdal -ldl -lfftw3 -lfftw3_omp -lgsl -lgslcblas -lproj
+       g++ -fopenmp -m64 -o $(BUILDDIR)/mialisp $(BUILDOBJS) $(LIBMIALDIR)/libmiallib_xlisp.a -lz -ltiff -lgeotiff -lgdal -ldl -lfftw3 -lfftw3_omp -lgsl -lgslcblas -lproj -lshp
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

