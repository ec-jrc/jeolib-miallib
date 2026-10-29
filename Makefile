#
# build requires documentation to be generated for core and
# documentation generation for core requires doxygen to be installed!
#
build:
	cd core/c && make -j build
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


