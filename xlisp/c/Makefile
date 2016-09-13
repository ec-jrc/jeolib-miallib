

CC =gcc -m64 -O3 -Wall # -Or26 # -pg

BUILDDIR = ../build
LIBMIADIR= ../../core/build/lib

SRCS = xlisp.c xlbfun.c xlcont.c xldbug.c xldmem.c xleval.c \
	xlfio.c xlglob.c xlimage.c xlinit.c xlio.c xljump.c xllist.c \
	xlmath.c xlobj.c xlpp.c xlprin.c xlread.c xlstr.c xlsubr.c \
	xlsym.c xlsys.c unixstuf.c unixprim.c xlseq.c xlstruct.c xlftab.c xlmath2.c \
	xlmath3.c xlbignum.c xlglue1.c

OBJS = $(SRCS:.c=.o)


BUILDOBJS =  $(SRCS:%.c=$(BUILDDIR)/%.o)


INCLUDE=-I.

CFLAGS = -DXLISP -DPOSIX -DUNIX -DEDITOR -DMIAL -DALLFUNCTIONS -DTEST2 -DDOMINIK -DMARCIN -DTESTING



$(BUILDDIR)/miaxlisp: banner_mialib.h $(BUILDOBJS)
	g++ -fopenmp -m64 -o $(BUILDDIR)/miaxlisp $(BUILDOBJS) $(LIBMIADIR)/libmia_xlisp.a -lgdal -lopenjp2 -ljpeg -lgeotiff -ltiff -lz -lproj -lpng -lgsl -lgslcblas -lexpat -lgif -lpq -lssl -lcrypto -lcurl -lfftw3_omp -lfftw3 -ldl -lm -lsqlite3 -lcrypt -lxml2 -lgeos_c
	#g++ -fopenmp -m64 -o $(BUILDDIR)/miaxlisp $(BUILDOBJS) $(LIBMIADIR)/libmia_xlisp.a -lgdal -lopenjp2 -ljpeg -lgeotiff -ltiff -lz -lproj -lpng -lgsl -lgslcblas -lexpat -lgif -lpq -lssl -lcrypto /usr/local/lib/libnn.a /usr/local/lib/libcsa.a -lcurl -lfftw3_omp -lfftw3 -ldl -lm -lsqlite3 -lcrypt -lxml2 -lgeos_c


banner_mialib.h: $(SRCS)
	echo "#define BANNER_MIALIB \"Version as of `date`\"" > banner_mialib.h;


$(BUILDDIR)/xlftab.o : xlftab.c osdefs.h osptrs.h
$(BUILDDIR)/xlglue1.o : xlglue1.h

$(BUILDDIR)/%.o: %.c xlisp.h xlftab.h xlglob.h
	@mkdir -p $(@D)
	$(CC) -c $(CFLAGS) $(INCLUDE) -o $@ $<


all: $(BUILDDIR)/miaxlisp

clean:
	test -d $(BUILDDIR) && find $(BUILDDIR) -delete
	test -f banner_mialib.h && rm banner_mialib.h

