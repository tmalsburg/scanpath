build: scanpath_1.05.tar.gz

documentation: R/scanpath.R
	R -e 'library(roxygen2); roxygenize()'

scanpath_1.05.tar.gz: scanpath/R/scanpath.R scanpath/R/fixsel.R scanpath/src/cscasim.c scanpath/src/fixsel.cpp scanpath/src/fixsel.hpp scanpath/DESCRIPTION scanpath/NAMESPACE scanpath/data/eyemovements.rda
	R CMD build scanpath

check: scanpath_1.05.tar.gz

scanpath.Rcheck/00check.log: scanpath_1.05.tar.gz
	R CMD check scanpath_1.05.tar.gz

install: check
	R CMD INSTALL scanpath_1.05.tar.gz
