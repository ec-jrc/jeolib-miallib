python_version_full := $(wordlist 2,4,$(subst ., ,$(shell python3 --version 2>&1)))
python_version_major := $(word 1,${python_version_full})
python_version_minor := $(word 2,${python_version_full})
python_version_patch := $(word 3,${python_version_full})

PYVER=${python_version_major}.${python_version_minor}
PYVERSHORT = ${python_version_major}

#
# build requires documentation to be generated for core and
# documentation generation for core requires doxygen to be installed!
#
build:
	cd core/c && make -j build && make -j doc
	cd xlisp/c && make -j all

generic:
	cd core/c && make -j generic


#
# documentation generation for python requires epydoc to be installed!
# documentation generation of xlisp part requires latex, pdflatex, and latex2hmtl to be installed!
#
doc:
	cd core/c && make doc
	cd xlisp/doc && make -j 1 all

all: build doc


install:
	cd core/c && make install
	cd xlisp/c && make install
	cd xlisp/doc && make install

install-generic:
	cd core/c && make install

uninstall:
	cd core/c && make uninstall
	cd xlisp/c && make uninstall
	cd xlisp/doc && make uninstall

uninstall-generic:
	cd core/c && make uninstall

clean:
	-cd core/c && make clean
	-cd xlisp/c && make clean
	-cd xlisp/doc && make clean


