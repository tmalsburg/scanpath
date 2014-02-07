
MANPAGES=scanpath/man/constant.vars.Rd scanpath/man/eyemovements.Rd scanpath/man/find.fixation.Rd scanpath/man/inverse.gnomonic.Rd scanpath/man/match.scanpaht.Rd scanpath/man/replace.syms.Rd scanpath/man/scanpath.Rd scanpath/man/scasim.Rd
RSOURCES=scanpath/R/fixsel.R scanpath/R/plot_scanpaths.R scanpath/R/scanpath.R
CSOURCES=scanpath/src/cscasim.c scanpath/src/fixsel.cpp scanpath/src/fixsel.hpp

scanpath_1.05.tar.gz: $(RSOURCES) $(CSOURCES) documentation scanpath/DESCRIPTION scanpath/NAMESPACE
	R CMD build scanpath

documentation: $(RSOURCES)
	cd scanpath; R -e 'library(roxygen2); roxygenize()'

check: $(RSOURCES) $(CSOURCES) documentation scanpath/DESCRIPTION scanpath/NAMESPACE
	R CMD check scanpath

install: check scanpath_1.05.tar.gz
	R CMD INSTALL scanpath_1.05.tar.gz
