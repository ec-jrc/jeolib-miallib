
build:	
	cd core/c && make -j all && make -j doc
	cd swig/python && make -j 1 all
	cd xlisp/c && make -j all



#documentation generation of xlisp part requires manual intervention due to unsolved proble with latex source
#try cd xlisp/doc && make 
# followed by carriage returns to ignore all errors

install: build
	cd core/c && make install
	cd swig/python && make install

clean:
	-cd core/c && make clean
	-cd swig/python && make veryclean
	-cd xlisp/c && make clean


