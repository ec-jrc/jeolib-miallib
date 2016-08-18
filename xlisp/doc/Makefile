# 
# Makefile for generating LaTeX documents
# send comments and suggestions to Pierre.Soille@jrc.it
#


# insert the filename of your filename.tex
FN = miadocxlisp

########################################################
# one should not need to edit this file beyond this line

GPS_FILES = $(GPLOT_FILES:%.gnu=%.gps)
EPS_FILES = $(TIFF_FILES:%.tif=%.eps) 
XPS_FILES = $(XFIG_FILES:%.fig=%.xps)


SRCSDIR = ../../core/c
SRCXLISPDIR = ../c
BUILDDIR = ../build/doc/texinputs




SRC = `find $(SRCSDIR) -name "*.c"`


LATEX_FILES = dem.tex filter.tex  geodesy.tex       hmt.tex     init.tex   limits.tex  pointop.tex  shape.tex \
	distance.tex  flow.tex    geometry.tex      idea.tex    io.tex     lut.tex     remsens.tex \
	erodil.tex    format.tex  geometry_tmp.tex  imstat.tex  label.tex  opclo.tex   segment.tex contrast.tex


${FN}:  ${FN}.dvi

${FN}.dvi: xlglue1.tex cfit.tex cfet.tex cfvt.tex $(LATEX_FILES) $(FN)
	latex ${FN}; bibtex ${FN}; latex ${FN}; cat ${FN}.idx  | sed 's/indexentry{\([@*]\)\([a-z]*\)/indexentry{\2\@\\texttt{\"\1}\2/' | tr -d '  ' > /tmp/toto.idx;  mv /tmp/toto.idx ${FN}.idx; makeindex ${FN}.idx; latex ${FN}; touch ${FN}.bbl; touch $@

$(FN).html: ${FN}.dvi
	/usr/local/bin/latex2html $(FN)

$(FN).pdf: ${FN}.dvi
	pdflatex $(FN)



xlglue1.tex: $(SRCXLISPDIR)/xlglue1.c
	@mkdir -p $(BUILDDIR)
	echo $(LSP_FILES)
	cat  $(SRCXLISPDIR)/xlglue1.c | sed -n -f sedscr | sed -f sedscr2 > $(BUILDDIR)/xlglue1.tex

%.tex: ../lsp/%.lsp
	@mkdir -p $(BUILDDIR)
	cat $^ | sed -n -f sedlspscr | sed -f sedscr2 | sed -f sedscr3 > $(BUILDDIR)/$@

cfit.tex: $(BUILDDIR)/xlglue1.tex
	@mkdir -p $(BUILDDIR)
	./makecfit

cfet.tex: $(BUILDDIR)/xlglue1.tex
	@mkdir -p $(BUILDDIR)
	./makecfet

cfvt.tex: $(BUILDDIR)/xlglue1.tex
	@mkdir -p $(BUILDDIR)
	./makecfvt



all: ${FN} ${FN}.html ${FN}.pdf

clean:
	rm $(BUILDDIR)/*; rm -f ${FN}.aux ${FN}.log ${FN}.blg ${FN}.dvi ${FN}.toc ${FN}.idx ${FN}.ind ${FN}.bbl ${FN}.bib ${FN}.out ${FN}.ps $(EPS_FILES) $(XPS_FILES) core ; rm -rf .xvpics cfet.tex cfit.tex cfvt.tex
