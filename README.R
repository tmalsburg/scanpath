## Install


## [[file:README.org::*Install][Install:1]]
  library("devtools");
  install_github("tmalsburg/scanpath/scanpath", dependencies=TRUE)
## Install:1 ends here

## Usage example

## Let's have a look at the toy data:


## [[file:README.org::*Usage%20example][Usage\ example:1]]
library(scanpath)
data(eyemovements)
head(eyemovements)
## Usage\ example:1 ends here



## #+RESULTS:
## | subject | trial | word |      x |      y | duration |
## |---------+-------+------+--------+--------+----------|
## | Anne    |     1 |    1 |  48.82 | 385.01 |       97 |
## | Anne    |     1 |    1 |  43.47 | 383.14 |      222 |
## | Anne    |     1 |    3 |  131.4 | 387.77 |      147 |
## | Anne    |     1 |    2 | 106.22 | 385.94 |       88 |
## | Anne    |     1 |    3 | 165.26 | 386.75 |      156 |
## | Anne    |     1 |    4 | 167.25 | 389.51 |       34 |

## Next, we use a function from the package to plot the scanpaths in this data set.  In this plot, each panel shows the data from one trial.  There are three participants in this data set which coded by color.  The data is from a sentence reading task.  The x-axis shows words and the y-axis time in milliseconds.


## [[file:README.org::*Usage%20example][Usage\ example:2]]
plot_scanpaths(duration ~ word | trial, eyemovements, subject)
## Usage\ example:2 ends here



## #+RESULTS:
## [[file:Plots/scanpaths.png]]

## In the following plots, we plot the fixations using their x- and y-coordinates.  Each circle is a fixation and the size of the circle represents the duration of the corresponding fixation.


## [[file:README.org::*Usage%20example][Usage\ example:3]]
plot_scanpaths(duration ~ x + y | trial, eyemovements, subject)
## Usage\ example:3 ends here



## #+RESULTS:
## [[file:Plots/scanpaths2.png]]

## The function ~plot_scanpaths~ return a ggplot2 object.  This means that we can style the plot before rendering it. For example, we can change the limits of the axes:


## [[file:README.org::*Usage%20example][Usage\ example:4]]
library(ggplot2)
p <- plot_scanpaths(duration ~ x + y | trial, eyemovements, subject)
p + xlim(0, 600) + ylim(284, 484)
## Usage\ example:4 ends here



## #+RESULTS:
## [[file:Plots/scanpaths3.png]]

## Now, we calculate the pair-wise similarities of the nine scanpaths in the dataset using the scasim measure.  A simplifying intuition is that the measure quantifies the time that was spent looking at different things or at the same things in different order.  For details, see [[https://www.sciencedirect.com/science/article/pii/S0749596X11000179][von der Malsburg & Vasishth (2011)]].


## [[file:README.org::*Usage%20example][Usage\ example:5]]
d1 <- scasim(eyemovements, duration ~ x + y | trial, 512, 384, 60, 1/30, normalize=FALSE)
round(d1, 2)
## Usage\ example:5 ends here



## #+RESULTS:
## |   |     1 |     2 |     3 |     4 |     5 |     6 |     7 |     8 |     9 |
## |---+-------+-------+-------+-------+-------+-------+-------+-------+-------|
## | 1 |     0 | 29.87 | 54.41 | 10.53 |  35.3 | 60.26 | 13.76 | 38.72 | 65.47 |
## | 2 | 29.87 |     0 | 31.59 | 33.35 | 12.48 | 37.45 | 36.57 | 15.91 | 42.65 |
## | 3 | 54.41 | 31.59 |     0 | 57.89 | 37.02 | 13.42 | 61.11 | 40.45 | 18.62 |
## | 4 | 10.53 | 33.35 | 57.89 |     0 | 33.23 | 58.74 | 10.89 | 35.83 | 62.87 |
## | 5 |  35.3 | 12.48 | 37.02 | 33.23 |     0 | 34.91 | 35.16 | 10.83 | 37.57 |
## | 6 | 60.26 | 37.45 | 13.42 | 58.74 | 34.91 |     0 | 60.88 | 37.55 | 12.78 |
## | 7 | 13.76 | 36.57 | 61.11 | 10.89 | 35.16 | 60.88 |     0 | 34.74 | 61.21 |
## | 8 | 38.72 | 15.91 | 40.45 | 35.83 | 10.83 | 37.55 | 34.74 |     0 | 35.92 |
## | 9 | 65.47 | 42.65 | 18.62 | 62.87 | 37.57 | 12.78 | 61.21 | 35.92 |     0 |

## The time that was spent looking at different things of course depends on the duration of the two compared trials.  (total duration of the two compared scanpaths constitutes an upper bound).  This means that two long scanpaths may  have a larger dissimilarity than two shorter scanpaths even if they look more similar.  Depending on the research question, this may be undesirable.  One way to get rid of the trivial influence of total duration is to normalize the dissimilarity scores.  For example, we can divide them by the total duration of the two compared scanpaths:


## [[file:README.org::*Usage%20example][Usage\ example:6]]
d2 <- scasim(eyemovements, duration ~ x + y | trial, 512, 384, 60, 1/30, normalize="durations")
round(d2, 4)
## Usage\ example:6 ends here



## #+RESULTS:
## |   |      1 |      2 |      3 |      4 |      5 |      6 |      7 |      8 |      9 |
## |---+--------+--------+--------+--------+--------+--------+--------+--------+--------|
## | 1 |      0 | 0.0062 | 0.0098 | 0.0021 | 0.0063 | 0.0092 | 0.0024 | 0.0062 | 0.0087 |
## | 2 | 0.0062 |      0 | 0.0053 | 0.0062 | 0.0021 | 0.0053 |  0.006 | 0.0024 | 0.0054 |
## | 3 | 0.0098 | 0.0053 |      0 | 0.0094 | 0.0054 | 0.0017 | 0.0089 | 0.0054 | 0.0021 |
## | 4 | 0.0021 | 0.0062 | 0.0094 |      0 | 0.0053 | 0.0082 | 0.0017 | 0.0052 | 0.0078 |
## | 5 | 0.0063 | 0.0021 | 0.0054 | 0.0053 |      0 | 0.0045 | 0.0051 | 0.0014 | 0.0043 |
## | 6 | 0.0092 | 0.0053 | 0.0017 | 0.0082 | 0.0045 |      0 | 0.0077 | 0.0044 | 0.0013 |
## | 7 | 0.0024 |  0.006 | 0.0089 | 0.0017 | 0.0051 | 0.0077 |      0 | 0.0046 | 0.0069 |
## | 8 | 0.0062 | 0.0024 | 0.0054 | 0.0052 | 0.0014 | 0.0044 | 0.0046 |      0 | 0.0038 |
## | 9 | 0.0087 | 0.0054 | 0.0021 | 0.0078 | 0.0043 | 0.0013 | 0.0069 | 0.0038 |      0 |

## The number are much smaller now and can be interpreted as the proportion of time that was spent looking at different things.

## The numbers in the matrix above capture a lot of information about the scanpath variance in the data set.  However, dissimilarity scores are somewhat tricky to analyze.  One problem is that these values have strong statistical dependencies.  When we change on scanpath, this affects /n/ dissimilarity scores.  This has to be kept in mind when doing inferential stats directly on the dissimilarity scores.  While there are solutions for dealing with this issue, it can be more convenient to produce a representation that is free from this problem.  One way to do this is to calculate a so-called “map of scanpath space.”  On such a map every point represents a scanpath and the distances on the map reflect the dissimilarities in the above matrix.

## The method for calculating such maps is called multi-dimensional scaling and one version of the general idea is implemented in the function ~cmdscale~.


## [[file:README.org::*Usage%20example][Usage\ example:7]]
map1 <- cmdscale(d1)
round(map1, 5)
## Usage\ example:7 ends here



## #+RESULTS:
## |   |        V1 |        V2 |
## |---+-----------+-----------|
## | 1 |  29.19698 | -11.74914 |
## | 2 |   2.02624 |  -1.97714 |
## | 3 | -27.32948 |   -14.708 |
## | 4 |  28.81608 |  -2.61734 |
## | 5 |   0.50589 |   8.65206 |
## | 6 | -30.05024 |  -3.16703 |
## | 7 |  29.63311 |   4.10074 |
## | 8 |   0.22881 |  15.28742 |
## | 9 | -33.02739 |   6.17844 |

## A more sophisticated variant is provided by the function ~isoMDS~ in the package ~MASS~.


## [[file:README.org::*Usage%20example][Usage\ example:8]]
library(MASS)
map2 <- isoMDS(d1)
round(map2$points, 5)
## Usage\ example:8 ends here



## #+RESULTS:
## |   |        V1 |        V2 |
## |---+-----------+-----------|
## | 1 |  30.57631 |  -6.59302 |
## | 2 |   0.75934 |  -6.64837 |
## | 3 | -29.40076 | -13.33893 |
## | 4 |  30.85464 |  -0.06643 |
## | 5 |  -0.05598 |   1.58076 |
## | 6 | -31.56042 |  -0.46588 |
## | 7 |   31.0484 |   6.44287 |
## | 8 |   0.22446 |   8.10701 |
## | 9 | -32.44598 |  10.98198 |

## Every scanpath is now described by two variables, V1 and V2.  The stress value for this map is very low, which suggests that the map faithfully represents the variance in the dissimilarity scores.  (Stress values below 0.2 are considered acceptable and values below 0.1 are good.) 


## [[file:README.org::*Usage%20example][Usage\ example:9]]
round(map$stress, 3)
## Usage\ example:9 ends here



## #+RESULTS:
## 0.06

## This low stress value, effectively means that two dimensions are sufficient to describe the scanpath variance that is captured by the scasim measure.  This is not surprising because the scanpaths in the toy data set were designed to vary with respect to two properties: 1.) The speed of the reader, and 2.) whether there was a regression back to the beginning of the sentence and how long it was.  When we plot the map of scanpath space, we find that the two variables produced by ~isoMDS~ reflect precisely these properties:


## [[file:README.org::*Usage%20example][Usage\ example:10]]
plot(map1, cex=4)
text(map1, labels=rownames(map1))
## Usage\ example:10 ends here



## #+RESULTS:
## [[file:Plots/map_of_nine_scanpaths.png]]

## The benefit of the resulting representation of scanpaths is that it have much weaker statistical dependencies and that this representation is much suitable for all kinds of analyses.  For example, we choose among a large number of clustering algorithms to test whether there are groups of self-similar scanpaths in a data set.  Below, we use the k-means clustering algorithm (not necessarily recommended but simple):


## [[file:README.org::*Usage%20example][Usage\ example:11]]
set.seed(10)
clusters <- kmeans(map1, 3, iter.max=100)
plot(map1, cex=4, col=clusters$cluster, pch=19)
text(map1, labels=rownames(map1), col="white")
points(clusters$centers, col="blue", pch=3, cex=4)
## Usage\ example:11 ends here
