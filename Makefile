
MANPAGES=scanpath/man/avg.group.dist.Rd scanpath/man/constant.vars.Rd scanpath/man/eyemovements.Rd scanpath/man/find.fixation.Rd scanpath/man/inverse.gnomonic.Rd scanpath/man/replace.all.Rd scanpath/man/scanpath.Rd scanpath/man/scasim.Rd scanpath/man/which.centroid.Rd
RSOURCES=scanpath/R/fixsel.R scanpath/R/plot_scanpaths.R scanpath/R/scanpath.R
CSOURCES=scanpath/src/cscasim.c scanpath/src/fixsel.cpp scanpath/src/fixsel.hpp

scanpath_1.05.tar.gz: $(RSOURCES) $(CSOURCES) documentation.intermediate scanpath/DESCRIPTION scanpath/NAMESPACE
	R CMD build scanpath

check: $(RSOURCES) $(CSOURCES) $(MANPAGES) scanpath/DESCRIPTION scanpath/NAMESPACE
	R CMD check scanpath

manual: Documentation/scanpath-manual.pdf

Documentation/scanpath-manual.pdf: check
	cp ./scanpath.Rcheck/scanpath-manual.pdf Documentation/scanpath-manual.pdf

$(MANPAGES): documentation.intermediate
.INTERMEDIATE=documentation.intermediate
documentation.intermediate: $(RSOURCES)
	cd scanpath; R -e 'library(roxygen2); roxygenize()'

install: check scanpath_1.05.tar.gz
	sudo R CMD INSTALL scanpath_1.05.tar.gz
