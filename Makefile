

PYVER = 3.6
PYVERSHORT = 3

#
# build requires documentation to be generated for core and
# documentation generation for core requires doxygen to be installed!
#
build:
	cd core/c && make -e -j build && make -j doc
	cd swig/python && make -e -j 1 build
	cd xlisp/c && make -j all



#
# documentation generation for python requires epydoc to be installed!
# documentation generation of xlisp part requires latex, pdflatex, and latex2hmtl to be installed!
#
doc:
	if [ ${PYVERSHORT} -eq 2 ]; then cd swig/python && make -j 1 doc; fi
	if [ ${PYVERSHORT} -eq 3 ]; then cd swig/python/doc && make html; fi
	if [ ${PYVERSHORT} -eq 3 ]; then cd swig/python/doc && make latex; fi
	#cd xlisp/doc && make -j 1 all

all: build doc


install:
	cd core/c && make install
	cd swig/python && make -e install
	cd xlisp/c && make install
	#cd xlisp/doc && make install

uninstall:
	cd core/c && make uninstall
	cd swig/python && make uninstall
	cd xlisp/c && make uninstall
	cd xlisp/doc && make uninstall

clean:
	-cd core/c && make clean
	-cd swig/python && make clean
	-cd swig/python && make veryclean
	-cd xlisp/c && make clean
	-cd xlisp/doc && make clean


