

CC =gcc -m64 -O3 -Wall # -Or26 # -pg

PREFIX=/usr/local
BUILDDIR = ../build
LIBMIALDIR= ../../core/build/lib

SRCS = xlisp.c xlbfun.c xlcont.c xldbug.c xldmem.c xleval.c \
	xlfio.c xlglob.c xlimage.c xlinit.c xlio.c xljump.c xllist.c \
	xlmath.c xlobj.c xlpp.c xlprin.c xlread.c xlstr.c xlsubr.c \
	xlsym.c xlsys.c unixstuf.c unixprim.c xlseq.c xlstruct.c xlftab.c xlmath2.c \
	xlmath3.c xlbignum.c xlglue1.c

OBJS = $(SRCS:.c=.o)


BUILDOBJS =  $(SRCS:%.c=$(BUILDDIR)/%.o)


INCLUDE=-I.

CFLAGS = -DXLISP -DPOSIX -DUNIX -DEDITOR -DMIAL -DALLFUNCTIONS -DTEST2 -DMCISRG -DMARCIN -DTESTING



$(BUILDDIR)/mialisp: banner_miallib.h $(BUILDOBJS) $(LIBMIALDIR)/libmiallib_xlisp.a
	g++ -fopenmp -m64 -o $(BUILDDIR)/mialisp $(BUILDOBJS) $(LIBMIALDIR)/libmiallib_xlisp.a -lz -ltiff -lgeotiff -lgdal -ldl -lfftw3 -lfftw3_omp -lgsl -lgslcblas -lproj -lshp


banner_miallib.h: $(SRCS)
	echo "#define BANNER_MIALLIB \"Version as of `date`\"" > banner_miallib.h;


$(BUILDDIR)/xlftab.o : xlftab.c osdefs.h osptrs.h
$(BUILDDIR)/xlglue1.o : xlglue1.h

$(BUILDDIR)/%.o: %.c xlisp.h xlftab.h xlglob.h
	@mkdir -p $(@D)
	$(CC) -c $(CFLAGS) $(INCLUDE) -o $@ $<


all: $(BUILDDIR)/mialisp


install:
	cp -a $(BUILDDIR)/mialisp $(PREFIX)/bin/
	mkdir -p $(PREFIX)/share/mialisp/
	cp -ar ../lsp $(PREFIX)/share/mialisp/

uninstall:
	-rm -f $(PREFIX)/bin/mialisp
	-rm -rf $(PREFIX)/share/mialisp


clean:
	test -d $(BUILDDIR) && find $(BUILDDIR) -delete
	test -f banner_miallib.h && rm banner_miallib.h

