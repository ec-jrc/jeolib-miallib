Welcomne to miallib!

# Installation


## Build time dependencies

### Create a conda env with all necessary dependencies:

```
conda create --name mial --file conda-spec-file.txt
```

### Activate the mial conda env

```
conda activate mial
```

### Build miallib

```
cd core/c
make -j build
```

### Build mialisp
```
cd ../../xlisp/c
make build
make doc
```


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

