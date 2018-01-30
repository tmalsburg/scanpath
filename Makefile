
MANPAGES=scanpath/man/avg.group.dist.Rd scanpath/man/constant.vars.Rd scanpath/man/eyemovements.Rd scanpath/man/find.fixation.Rd scanpath/man/inverse.gnomonic.Rd scanpath/man/replace.all.Rd scanpath/man/scanpath.Rd scanpath/man/scasim.Rd scanpath/man/which.centroid.Rd
RSOURCES=scanpath/R/fixsel.R scanpath/R/plot_scanpaths.R scanpath/R/scanpath.R
CSOURCES=scanpath/src/cscasim.c

.PHONY: build clean install

build: scanpath_1.06.tar.gz

scanpath_1.06.tar.gz: $(RSOURCES) $(CSOURCES) documentation.intermediate scanpath/DESCRIPTION scanpath/NAMESPACE
	R CMD build scanpath

check: scanpath_1.06.tar.gz
	R CMD check --as-cran $<

manual: Documentation/scanpath-manual.pdf

Documentation/scanpath-manual.pdf: check
	cp ./scanpath.Rcheck/scanpath-manual.pdf Documentation/scanpath-manual.pdf

$(MANPAGES): documentation.intermediate
.INTERMEDIATE=documentation.intermediate
documentation.intermediate: $(RSOURCES)
	cd scanpath; R -e 'library(roxygen2); roxygenize()'

install: scanpath_1.06.tar.gz
	R CMD INSTALL scanpath_1.06.tar.gz
