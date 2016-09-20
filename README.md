# README #

## What is this repository for? ##

* This repository holds the aveytoolkit package. For details, see the [package description]((https://bitbucket.org/spa23/aveytoolkit-r-package/src/default/DESCRIPTION).

## How do I get set up? ##

### Dependencies ###

The following R packages are required for proper installation: `devtools` `ggplot2` `gplots` `limma`

Install these from the R prompt by running
```
#!R
## devtools, ggplot2, gplots
install.packages(c("devtools", "ggplot2", "gplots"))

## limma
source("https://bioconductor.org/biocLite.R")
biocLite("limma")
```

### Installation ###
Install from R using the `install_bitbucket` function provided by the `devtools` package.

```
#!R
library(devtools)
install_bitbucket(repo="spa23/aveytoolkit-r-package", ref="default")
```

## Where do I start? ##
For a detailed description of the functions in this package, see the [reference manual](https://bitbucket.org/spa23/aveytoolkit-r-package/raw/default/aveytoolkit.pdf).

You can also browse the documentation via HTML files after installing

```
#!R
library(aveytoolkit)
browseIndex("aveytoolkit")
```

## Who do I talk to? ##

* Stefan Avey <stefan.avey@yale.edu>
