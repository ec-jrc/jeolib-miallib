
BUILDDIR=../build/doc/

makebuilddir:
	test -d $(BUILDDIR) || make -p $(BUILDDIR)

doc:
	doxygen Doxyfile && ./doxy2swig.py  $(BUILDDIR)/xml/index.xml  $(BUILDDIR)/xml/mial_doxy2swig.i

clean: 
	find $(BUILDDIR)/ -delete; mkdir -p $(BUILDDIR)/


all: doc makebuilddir

