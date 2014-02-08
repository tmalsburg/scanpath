Scanpath
========

An R package providing tools for analyzing scanpaths found in eyetracking data.

Consult [von der Malsburg & Vasishth (2011)](https://www.sciencedirect.com/science/article/pii/S0749596X11000179) for details about this analysis method.  The manual of the package can be found [here](https://github.com/tmalsburg/scanpath/blob/master/Documentation/scanpath-manual.pdf?raw=true).  The package includes a simple toy dataset and examples.

    library(scanpath)
    data(eyemovements)

    # Calculate the pair-wise similarities of the nine scanpaths in
    # the dataset:

    d <- scasim(eyemovements, dur ~ x + y | trial, 512, 384, 60, 1/30)

    # Fit a map of "scanpath space":
	
    map <- cmdscale(d)

    # Plot the map:
    plot(map, cex=4)
    text(map, labels=rownames(map))

![Map of scanpath space for toy dataset](https://raw.github.com/tmalsburg/scanpath/master/Screenshots/map_of_nine_scanpaths.png)
