# aveytoolkit R package #

[![Build Status](https://travis-ci.org/stefanavey/aveytoolkit.svg?branch=master)](https://travis-ci.org/stefanavey/aveytoolkit)

## What is this repository for? ##

* This repository holds the aveytoolkit package. It is a collection of miscellaneous helper functions.

## How do I get set up? ##

### Dependencies ###

The following R packages are required for proper installation: `remotes` `ggplot2` `gplots`.

Install these from the R prompt by running

```
install.packages(c("ggplot2", "gplots"))

```

### Installation ###
Install from R using the `install_github` function provided by the `remotes` package.

```

library(remotes)
install_github(repo="stefanavey/aveytoolkit")

```

## Where do I start? ##
For a detailed description of the functions in this package, see the [reference manual](https://github.com/stefanavey/aveytoolkit/raw/master/aveytoolkit.pdf).

You can also browse the documentation via HTML files after installing

```

library(aveytoolkit)
browseIndex("aveytoolkit")

```
