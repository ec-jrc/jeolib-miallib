

#
# build requires documentation to be generated for core and
# documentation generation for core requires doxygen to be installed!
#
build:
	cd core/c && make -j build && make -j doc
	cd swig/python && make -j 1 build
	cd xlisp/c && make -j all



#
# documentation generation for python requires epydoc  to be installed!
# documentation generation of xlisp part requires latex, pdflatex, and latex2hmtl to be installed!
#
doc:
	cd swig/python && make -j 1 doc
	cd xlisp/doc && make -j 1 all

all: build doc


install:
	cd core/c && make install
	cd swig/python && make install
	cd xlisp/c && make install

clean:
	-cd core/c && make clean
	-cd swig/python && make veryclean
	-cd xlisp/c && make clean
	-cd xlisp/doc && make clean


