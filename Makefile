build: scanpath_1.05.tar.gz

documentation: R/scanpath.R
	R -e 'library(roxygen2); roxygenize()'

scanpath_1.05.tar.gz: R/scanpath.R R/fixsel.R src/cscasim.c src/fixsel.cpp src/fixsel.hpp DESCRIPTION NAMESPACE data/eyemovements.rda
	R CMD build scanpath

check: scanpath.Rcheck/00check.log

scanpath.Rcheck/00check.log: scanpath_1.05.tar.gz
	R CMD check scanpath_1.05.tar.gz

install: check
	R CMD INSTALL scanpath_1.05.tar.gz
