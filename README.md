Welcome to miallib!

# Installation

## Create a conda env with all necessary dependencies:

```
conda create --name mial --file conda-spec-file.txt
```

## Activate the mial conda env

```
conda activate mial
```

## Build miallib and mialisp

```
make all
```

# Use mialisp

```
mialisp ~/git/mial/xlisp/build/mialisp ~/git/mial/xlisp/lsp/init.lsp
```

The doc is available here: `~/git/mial/xlisp/doc/mialdocxlisp.pdf`

# Use miallib

You can also try to bind the miallib libraries directly with your preferred software:
```
/usr/local/lib/libmiallib_generic.so
```

The doc is available here: `~/mial/core/build/doc/latex/miallib.pdf`

