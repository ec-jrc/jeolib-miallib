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
BUILDDOCDIR = ../build/doc
BUILDDOCDIRTEX = $(BUILDDOCDIR)/texinputs
BUILDDOCDIRHTML = $(BUILDDOCDIR)/html

export TEXINPUTS:=.:../build/doc/texinputs/:${TEXINPUTS}

SRC = `find $(SRCSDIR) -name "*.c"`


LATEX_FILES = $(BUILDDOCDIRTEX)/dem.tex $(BUILDDOCDIRTEX)/filter.tex  $(BUILDDOCDIRTEX)/geodesy.tex  \
	$(BUILDDOCDIRTEX)/hmt.tex     $(BUILDDOCDIRTEX)/init.tex   $(BUILDDOCDIRTEX)/limits.tex \
	$(BUILDDOCDIRTEX)/pointop.tex  $(BUILDDOCDIRTEX)/shape.tex \
	$(BUILDDOCDIRTEX)/distance.tex  $(BUILDDOCDIRTEX)/flow.tex    $(BUILDDOCDIRTEX)/geometry.tex \
	$(BUILDDOCDIRTEX)/idea.tex    $(BUILDDOCDIRTEX)/io.tex     $(BUILDDOCDIRTEX)/lut.tex  \
	$(BUILDDOCDIRTEX)/remsens.tex $(BUILDDOCDIRTEX)/erodil.tex    $(BUILDDOCDIRTEX)/format.tex \
	$(BUILDDOCDIRTEX)/geometry_tmp.tex  $(BUILDDOCDIRTEX)/imstat.tex  $(BUILDDOCDIRTEX)/label.tex  \
	$(BUILDDOCDIRTEX)/opclo.tex  $(BUILDDOCDIRTEX)/segment.tex $(BUILDDOCDIRTEX)/contrast.tex


${FN}.dvi: $(BUILDDOCDIRTEX)/xlglue1.tex $(BUILDDOCDIRTEX)/cfit.tex $(BUILDDOCDIRTEX)/cfet.tex $(BUILDDOCDIRTEX)/cfvt.tex $(LATEX_FILES) $(FN).tex
	latex ${FN}; bibtex ${FN}; latex ${FN}; cat ${FN}.idx  | sed 's/indexentry{\([@*]\)\([a-z]*\)/indexentry{\2\@\\texttt{\"\1}\2/' | tr -d '  ' > /tmp/toto.idx;  mv /tmp/toto.idx ${FN}.idx; makeindex ${FN}.idx; latex ${FN}; touch ${FN}.bbl; touch $@

$(BUILDDOCDIRHTML)/$(FN).html: ${FN}.dvi
	/usr/local/bin/latex2html $(FN)

$(FN).pdf: ${FN}.dvi
	pdflatex $(FN)



$(BUILDDOCDIRTEX)/xlglue1.tex: $(SRCXLISPDIR)/xlglue1.c
	@mkdir -p $(BUILDDOCDIRTEX)
	echo $(LSP_FILES)
	cat  $(SRCXLISPDIR)/xlglue1.c | sed -n -f sedscr | sed -f sedscr2 > $(BUILDDOCDIRTEX)/xlglue1.tex

$(BUILDDOCDIRTEX)/%.tex: ../lsp/%.lsp
	@mkdir -p $(BUILDDOCDIRTEX)
	cat $^ | sed -n -f sedlspscr | sed -f sedscr2 | sed -f sedscr3 > $@

$(BUILDDOCDIRTEX)/cfit.tex: $(BUILDDOCDIRTEX)/xlglue1.tex
	@mkdir -p $(BUILDDOCDIRTEX)
	./makecfit

$(BUILDDOCDIRTEX)/cfet.tex: $(BUILDDOCDIRTEX)/xlglue1.tex
	@mkdir -p $(BUILDDOCDIRTEX)
	./makecfet

$(BUILDDOCDIRTEX)/cfvt.tex: $(BUILDDOCDIRTEX)/xlglue1.tex
	@mkdir -p $(BUILDDOCDIRTEX)
	./makecfvt



all: ${FN}.dvi $(BUILDDOCDIRHTML)/${FN}.html ${FN}.pdf

clean:
	rm -rf $(BUILDDOCDIR); rm -f ${FN}.aux ${FN}.ilg $(FN).pdf ${FN}.log ${FN}.blg ${FN}.dvi ${FN}.toc ${FN}.idx ${FN}.ind ${FN}.bbl ${FN}.out ${FN}.ps $(EPS_FILES) $(XPS_FILES) core ; rm -rf .xvpics cfet.tex cfit.tex cfvt.tex
