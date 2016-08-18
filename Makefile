


build:	
	cd core/c && make -j all && make -j doc
	cd swig/python && make all
	cd xlisp/c && make -j all



#documentation generation of xlisp part requires manual intervention due to unsolved proble with latex source
#try cd xlisp/doc && make 
# followed by carriage returns to ignore all errors



clean:
	-cd core/c && make clean
	-cd swig/python && make veryclean
	-cd xlisp/c && make clean


