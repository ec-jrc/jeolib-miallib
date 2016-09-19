
build:	
	cd core/c && make -j all && make -j doc
	cd swig/python && make -j 1 all
	cd xlisp/c && make -j all
	cd xlisp/doc && make -j 1 all

#documentation generation of xlisp part requires latex, pdflatex, and latex2hmtl to be installed!


install: build
	cd core/c && make install
	cd swig/python && make install
	cd xlisp/c && make install

clean:
	-cd core/c && make clean
	-cd swig/python && make veryclean
	-cd xlisp/c && make clean
	-cd xlisp/doc && make clean


