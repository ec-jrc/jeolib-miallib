
BUILDDIR=../build/doc/
PREFIX=/usr/local

INSTALLDOCDIR=${PREFIX}/share/doc/miallib/lib/

doc: Doxyfile
	mkdir -p $(BUILDDIR)
	doxygen Doxyfile && ./doxy2swig.py -t $(BUILDDIR)/xml/index.xml  $(BUILDDIR)/xml/mial_doxy2swig.i
	cd ../build/doc/latex && make all && mv refman.pdf miallib.pdf

clean: 
	find $(BUILDDIR)/ -delete; mkdir -p $(BUILDDIR)/


all: doc

install:
	mkdir -p ${INSTALLDOCDIR}/pdf && cp -a ../build/doc/latex/miallib.pdf ${INSTALLDOCDIR}/pdf/

uninstall:
	-rm -rf ${INSTALLDOCDIR}/pdf
	-rmdir ${INSTALLDOCDIR}

